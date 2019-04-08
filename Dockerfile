FROM moskitos/rabbitmq-public-umbrella
COPY *sh /var/opt/
WORKDIR /var/opt/amqp-filter-exchange
CMD ["/var/opt/compile.sh"]