<?php declare(strict_types=1);

require '../../vendor/autoload.php';

use Bunny\Channel;
use Bunny\Client;
use Bunny\Message;
use Dotenv\Dotenv;
use PHPMailer\PHPMailer\PHPMailer;
use PHPMailer\PHPMailer\SMTP;

Dotenv::createUnsafeImmutable(__DIR__ . '/../../')->load();

$connection = [
  'host' => $_ENV["RB_HOST"],
  'vhost' => $_ENV["RB_VHOST"],
  'user' => $_ENV["RB_USER"],
  'password' => $_ENV["RB_PASSWORD"],
];

$mail = new PHPMailer();
$mail->IsSMTP();

$mail->SMTPDebug = true;
$mail->SMTPAuth = true;
$mail->SMTPSecure = PHPMailer::ENCRYPTION_SMTPS;

$mail->Port = $_ENV["SMTP_PORT"];
$mail->Host = $_ENV["SMTP_HOST"];
$mail->Username = $_ENV["SMTP_USER"];
$mail->Password = $_ENV["SMTP_PASSWORD"];

$bunny = new Client($connection);
$bunny->connect();

$channel = $bunny->channel();
$channel->queueDeclare($_ENV["NOTIFYING_QUEUE"]);
$channel->queueDeclare($_ENV["LOGGING_QUEUE"]);

$publish_function = function ($args) use($channel)
{
  $channel->publish(json_encode([
    "type" => $args["type"],
    "message" => $args["message"],
    "date" => date("Y-m-d H:i:s")
  ]), [], "", $_ENV["LOGGING_QUEUE"]);
};

$send_mail_function = function ($args) use($mail, $publish_function)
{
  $mail->setFrom($_ENV["SMTP_USER"], "");
  $mail->addAddress($args["to"]);

  $mail->isHTML(true);
  $mail->Subject = $args["subject"];
  $mail->Body = $args["message"];

  try {
    $mail->send();

    $publish_function([
      "type" => "info",
      "message" => "Sent email. Queue - {$_ENV['LOGGING_QUEUE']}. Id application - {$_ENV['ID_APPLICATION']}. Payload - {$args['to']}"
    ]);
  } catch (Exception $e) {
    $publish_function([
      "type" => "warning",
      "message" => "{$mail->ErrorInfo}. Queue - {$_ENV['LOGGING_QUEUE']}. Id application - {$_ENV['ID_APPLICATION']}. Payload - {$args['to']}"
    ]);
  }
};

$channel->run(
  function (Message $message, Channel $channel, Client $bunny) use($publish_function, $send_mail_function) {
    $obj = json_decode($message->{'content'});
    if(!is_object($obj)) {
      $publish_function([
        "type" => "info",
        "message" => "Invalid json. Queue - {$_ENV['LOGGING_QUEUE']}. Id application - {$_ENV['ID_APPLICATION']}. Payload - {$message->{'content'}}"
      ]);
    }

    if (is_object($obj)) {
      if(!isset($obj->{'email'}) || !isset($obj->{'subject'}) || !isset($obj->{'message'})) {
        $publish_function([
          "type" => "info",
          "message" => "Invalid json. Queue - {$_ENV['LOGGING_QUEUE']}. Id application - {$_ENV['ID_APPLICATION']}. Payload - {$message->{'content'}}"
        ]);
      }

      if (isset($obj->{'email'}) && isset($obj->{'subject'}) && isset($obj->{'message'})) {
        $result_validation = filter_var($obj->{'email'}, FILTER_VALIDATE_EMAIL);

        if (!$result_validation) {
          $publish_function([
            "type" => "info",
            "message" => "Invalid email address. Queue - {$_ENV['LOGGING_QUEUE']}. Id application - {$_ENV['ID_APPLICATION']}. Payload - {$message->{'content'}}"
          ]);
        }

        if ($result_validation) {
          $send_mail_function([
            "to" => $obj->{"email"},
            "subject" => $obj->{"subject"},
            "message" => $obj->{"message"}
          ]);
        }
      }
    }

    $channel->ack($message);
  },
  $_ENV["NOTIFYING_QUEUE"]
);
