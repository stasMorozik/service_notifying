<?php declare(strict_types=1);

require '../vendor/autoload.php';

use React\ChildProcess\Process;
use React\EventLoop\Loop;
use Dotenv\Dotenv;

Dotenv::createUnsafeImmutable(__DIR__ . '/../')->load();

$is_isset = isset($_ENV["PROCESSES"]);

if(!$is_isset) {
  die("Invalid number of processes");
}

if ($is_isset) {
  $PROCESSES = (int) $_ENV["PROCESSES"];

  do {
    $process = new React\ChildProcess\Process('php '.__DIR__.'/process/amqp.php');
    $process->start();

    $PROCESSES = $PROCESSES - 1;
  } while ($PROCESSES > 0);
}
