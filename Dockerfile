FROM moskitos/rabbitmq-public-umbrella
COPY *sh /var/opt/
WORKDIR /var/opt/amqp-filter-exchange
RUN chmod +x /var/opt/compile.sh
CMD ["/var/opt/compile.sh"]