FROM php:8.2-cli
FROM composer:latest

WORKDIR /service_notifying

COPY . .

RUN COMPOSER_VENDOR_DIR="/vendor" composer install

CMD ["php", "src/app.php"]
