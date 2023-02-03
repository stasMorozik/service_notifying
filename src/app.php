<?php declare(strict_types=1);

require '../vendor/autoload.php';

use React\ChildProcess\Process;
use React\EventLoop\Loop;
use Dotenv\Dotenv;

$loop = React\EventLoop\Loop::get();

$processes = [];

$timer = $loop->addPeriodicTimer(3.0, function ($timer) use ($loop, &$processes) {
  Dotenv::createUnsafeImmutable(__DIR__ . '/../')->load();
  if(!isset($_ENV["PROCESSES"])) {
    $loop->cancelTimer($timer);
  } else {
    $PROCESSES = (int) $_ENV["PROCESSES"];
    if (!$PROCESSES) {
      $loop->cancelTimer($timer);
    } else {
      echo count($processes) . PHP_EOL;
      if (count($processes) > $PROCESSES) {
        $last_process = array_pop($processes);
        $last_process->terminate();
      }

      if (count($processes) < $PROCESSES) {
        $process = new React\ChildProcess\Process('php '.__DIR__.'/process/amqp.php');
        $processes[] = $process;
        $process->start();
      }
    }
  }
});

$loop->run();
