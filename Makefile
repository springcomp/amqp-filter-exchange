PROJECT = amqp_filter_exchange
PROJECT_DESCRIPTION = A sql92 filter exchange
RABBITMQ_PLUGINS_DIR = /usr/lib/rabbitmq/plugins

define PROJECT_APP_EXTRA_KEYS
	{broker_version_requirements, []}
endef

DEPS = rabbit_common rabbit amqp_filter
dep_amqp_filter = hex 0.3.7
LOCAL_DEPS = action_reader

TEST_DEPS = rabbitmq_ct_helpers rabbitmq_ct_client_helpers amqp_client

DEP_EARLY_PLUGINS = rabbit_common/mk/rabbitmq-early-plugin.mk
DEP_PLUGINS = rabbit_common/mk/rabbitmq-plugin.mk

deploy: 
	rm -rf $(RABBITMQ_PLUGINS_DIR)
	mkdir $(RABBITMQ_PLUGINS_DIR)
	cp plugins/*.ez $(RABBITMQ_PLUGINS_DIR)

# FIXME: Use erlang.mk patched for RabbitMQ, while waiting for PRs to be
# reviewed and merged.

ERLANG_MK_REPO = https://github.com/rabbitmq/erlang.mk.git
ERLANG_MK_COMMIT = rabbitmq-tmp

include rabbitmq-components.mk
include erlang.mk