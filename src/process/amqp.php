<?php declare(strict_types=1);

require '../../vendor/autoload.php';

use Bunny\Channel;
use Bunny\Client;
use Bunny\Message;
use Dotenv\Dotenv;
use PHPMailer\PHPMailer\PHPMailer;
use PHPMailer\PHPMailer\SMTP;
use PHPMailer\PHPMailer\Exception;

Dotenv::createUnsafeImmutable(__DIR__ . '/../../')->load();

$connection = [
  'host' => $_ENV["RB_HOST"],
  'vhost' => $_ENV["RB_VHOST"],
  'user' => $_ENV["RB_USER"],
  'password' => $_ENV["RB_PASSWORD"],
];

$bunny = new Client($connection);
$bunny->connect();

$channel = $bunny->channel();
$channel->queueDeclare($_ENV["NOTIFYING_QUEUE"]);
$channel->queueDeclare($_ENV["LOGGING_QUEUE"]);

function publish($args)
{
  $channel->publish(json_encode([
    "type" => $args["type"],
    "message" => $args["message"],
    "date" => date("Y-m-d H:i:s")
  ]), [], "", $_ENV["LOGGING_QUEUE"]);
}

function send_email($to, $subject, $message)
{
  $mail = new PHPMailer(true);
  $mail->CharSet = "UTF-8";

  $mail->isSMTP();
  $mail->Host       = $_ENV["SMTP_HOST"];
  $mail->SMTPAuth   = true;
  $mail->Username   = $_ENV["SMTP_USER"];
  $mail->Password   = $_ENV["SMTP_PASSWORD"];
  $mail->Port       = $_ENV["SMTP_PORT"];

  $mail->setFrom($_ENV["SMTP_HOST"], $_ENV["SMTP_HOST"]);
  $mail->addAddress($to);

  $mail->isHTML(true);
  $mail->Subject = $subject;
  $mail->Body = $message;

  $mail->send();
}

$channel->run(
  function (Message $message, Channel $channel, Client $bunny) {
    $obj = json_decode($message->{'content'});
    if(!is_object($obj)) {
      publish([
        "type" => "info",
        "message" => "Invalid json. Queue - {$_ENV['LOGGING_QUEUE']}. Id application - {$_ENV['ID_APPLICATION']}. Payload - {$message->{'content'}}"
      ]);
    }

    if (is_object($obj)) {
      if(!$obj->{'email'} || !$obj->{'subject'} || !$obj->{'message'}) {
        publish([
          "type" => "info",
          "message" => "Invalid json. Queue - {$_ENV['LOGGING_QUEUE']}. Id application - {$_ENV['ID_APPLICATION']}. Payload - {$message->{'content'}}"
        ]);
      }

      if ($obj->{'email'} && $obj->{'subject'} && $obj->{'message'}) {
        $result_validation = filter_var($obj->{'email'}, FILTER_VALIDATE_EMAIL);

        if (!$result_validation) {
          publish([
            "type" => "info",
            "message" => "Invalid email address. Queue - {$_ENV['LOGGING_QUEUE']}. Id application - {$_ENV['ID_APPLICATION']}. Payload - {$message->{'content'}}"
          ]);
        }

        if ($result_validation) {
          send_email($obj->{'email'}, $obj->{'subject'}, $obj->{'message'});

          publish([
            "type" => "info",
            "message" => "Sent email. Queue - {$_ENV['LOGGING_QUEUE']}. Id application - {$_ENV['ID_APPLICATION']}. Payload - {$message->{'content'}}"
          ]);
        }
      }
    }
    $channel->ack($message);
  },
  $_ENV["NOTIFYING_QUEUE"]
);
