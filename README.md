# AMQP Filter Exchange Plugin

This plugin adds a new exchange type to [RabbitMQ](http://www.rabbitmq.com/) providing a new routing algorithm based on the evaluation of SQL92 predicates. 
Also allows to modify the message headers.

## Routing algorihtm

The name of the new exchange type is _amqp-filter_.     
Like other exchange types, the binding routing key is used by the routing algorithm. 
The binding routing key must be a valid predicate (SQL-like conditional expression). 
This predicate may contain properties that must be present in the message header fields.    
Due to the binding routing key property is limited to 256 characters, **the binding routing key must be specified in the binding arguments**. The binding arguments are a list of key-value pairs. The argument used to specify the routing key must have a key with the text `routingkey`, and a valid predicate as value.    
An exchange of type _amqp-filter_ will route a message to a queue if the queue has bound to the exchange with
a binding routing key in form of predicate and the evaluation of this predicate using the message header fields returns _true_.     
The message routing key is ignored.     

The predicate must follow the BNF grammar specified [here](https://docs.microsoft.com/en-us/azure/service-bus-messaging/service-bus-messaging-sql-filter).      
Example of valid predicate (binding routing key):
```
"UserName = 'Nick' And Age > 18"
```

The evaluation of this predicate will return true (and the message will be routed to the corresponding queue) 
if there are two fields in the message headers with names `UserName` and `Age`, and values "Nick" (_string_ type) and 18 (_number_ type) respectively.

## Modifying the message headers

This plugin provides also a feature that allow to execute some _actions_ that modify the message headers. 
The _actions_ use a SQL like expression with a similar syntax to the SQL UPDATE statement. 
These _actions_ must be specified in a binding argument with name _actions_ and type _string_.        
Example of valid _actions_:
```
Set LastName = 'Miller'; Set CompanyName = 'Twitter';
```
These _actions_ will add (or update if the property exists) these two fields to the message header. 
These _action_ are performed on the message inmediately before the message is routed to each queue. 

## Usage

### Erlang/OTP and RabbitMQ versions

The actual version of this plugin uses Erlang/OTP 20 and RabbitMQ 3.7.0.            

### RabbitMQ 3rd party plugin directories
RabbitMQ 3rd party [plugin directories](https://www.rabbitmq.com/plugins.html#plugin-directories) will differ from platform to platform and installation method to installation method.     
Plugin directory can be located by executing the following command:
```
rabbitmqctl eval 'application:get_env(rabbit, plugins_dir).'
{ok,"/usr/lib/rabbitmq/plugins:/usr/lib/rabbitmq/lib/rabbitmq_server-3.7.7/plugins"}
```
The first directory in the example above is the 3rd party plugin directory. You must set this value to the _RABBITMQ_PLUGINS_DIR_ variable in the project makefile. By default is `/usr/lib/rabbitmq/plugins`.
### Installation

```
make dist 
# generates the ez files in the /plugins folder.

make deploy
# copies the ez files to the RabbitMQ 3rd party plugin directory.
```

Now you can start the RabbitMQ server and enable the plugin:
```
rabbitmq-plugins enable amqp_filter_exchange
```
