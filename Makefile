APP=toy_flv_streaming

all: compile eunit                                                   

init:
	@eval "if ! [ -f 'src/${APP}.app.src' ]; then ./rebar create-app appid=${APP}; fi"
	@./rebar get-deps compile

compile:
	@./rebar compile skip_deps=true

xref:
	@./rebar xref skip_deps=true

clean:
	@./rebar clean skip_deps=true

eunit:
	@./rebar eunit skip_deps=true

edoc:
	@./rebar doc skip_deps=true

start:
	erl -sname $(APP)@localhost -pz ebin deps/*/ebin -eval 'erlang:display({start_app, $(APP), application:start($(APP))}).'

.dialyzer.plt:
	touch .dialyzer.plt
	dialyzer --build_plt --plt .dialyzer.plt --apps erts kernel stdlib

dialyze: .dialyzer.plt
	dialyzer --plt .dialyzer.plt -r ebin
