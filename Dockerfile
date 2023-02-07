FROM erlang:latest

WORKDIR /service_notifying

COPY . .

RUN rebar3 get-deps

CMD ["rebar3", "shell", "--sname", "service"]
