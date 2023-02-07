-module(amqp_super_supervisor).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},

  ChildSpecs = [
    #{
      id => amqp_supervisor,
      start => {amqp_supervisor, start_link, []},
      restart => permanent,
      shutdown => brutal_kill,
      type => supervisor,
      modules => [amqp_supervisor]
    }
  ],

  {ok, {SupFlags, ChildSpecs}}.
