-module(amqp_controller).

-behaviour(supervisor).

-export([start_link/1, publish/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include_lib("amqp_client/include/amqp_client.hrl").
-record(state, {channel, connection, queue, consumer_tag, socket_connections}).

start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

publish(Message) ->
	gen_server:call(?MODULE, {publish, Message}).

init(_Args) ->
	{ok, Connection} = amqp_connection:start(#amqp_params_network{
    username = application:get_env(service_notifying, rb_user),
    password = application:get_env(service_notifying, rb_password),
    virtual_host = "/",
    host = application:get_env(service_notifying, rb_host)
  }),

	{ok, Channel} = amqp_connection:open_channel(Connection),

	#'queue.declare_ok'{queue = Queue} = amqp_channel:call(Channel, #'queue.declare'{queue = application:get_env(service_notifying, logging_queue)}),

	#'basic.consume_ok'{consumer_tag = Tag} = amqp_channel:call(Channel, #'basic.consume'{queue = Queue}),

	{
    ok,
    #state{
      queue = Queue,
      channel = Channel,
      connection = Connection,
      consumer_tag = Tag, socket_connections = []
    }
  }.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast({publish, Message}, State) ->
  io:fwrite("~w",[Message]),
	{noreply, State};

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, #state{channel = Channel, consumer_tag = Tag,
	connection = Connection}) -> amqp_channel:call(Channel,#'basic.cancel'{consumer_tag = Tag}),
  amqp_connection:close(Connection),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
