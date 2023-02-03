<?php declare(strict_types=1);

require '../../vendor/autoload.php';

use Bunny\Channel;
use Bunny\Client;
use Bunny\Message;
use Dotenv\Dotenv;

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

$channel->run(
  function (Message $message, Channel $channel, Client $bunny) {
      print_r($message);
      $channel->ack($message);
  },
  $_ENV["NOTIFYING_QUEUE"]
);
