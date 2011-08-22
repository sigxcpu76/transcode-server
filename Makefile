REBAR:=./rebar
PATH=/opt/local/bin:/opt/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin

.PHONY: all erl test clean doc

all: erl

erl:
	$(REBAR) -j 1 -C rebar.config get-deps compile

erldebug:
	$(REBAR) -C rebar.debug.config compile

test: all
	@mkdir -p .eunit
	$(REBAR) skip_deps=true eunit

clean:
	$(REBAR) clean
	# -rm -rvf deps/*/ebin ebin doc .eunit

doc:
	$(REBAR) doc

run: all
	#erl -pa ebin -boot start_sasl -s rmqclient_app start
	#erl +W w -pa ebin -pa deps/*/ebin -boot start_sasl -kernel error_logger=silent -sasl sasl_error_logger=false -sasl errlog_type=error -s start_app
	erl -sname transcode -setcookie `cat priv/cookie` +W w -pa ebin -pa deps/*/ebin -config cf -boot start_sasl -sasl sasl_error_logger=false -eval "inets:start(), application:start(rfc4627_jsonrpc), application:start(transcode), reloader:start()"
	#erl +W w -pa ebin -pa deps/*/ebin -config cf -boot start_sasl -sasl sasl_error_logger=false -eval "reloader:start()"

daemon: all
	#erl -pa ebin -boot start_sasl -s rmqclient_app start
	#erl +W w -pa ebin -pa deps/*/ebin -boot start_sasl -kernel error_logger=silent -sasl sasl_error_logger=false -sasl errlog_type=error -s start_app
	erl -detached -name transcode -setcookie `cat priv/cookie` +W w -pa ebin -pa deps/*/ebin -config cf -boot start_sasl -sasl sasl_error_logger=false -eval "inets:start(), application:start(rfc4627_jsonrpc), application:start(transcode), reloader:start()"

debug: erldebug
	erl +W w -pa ebin -pa deps/*/ebin -config cf -boot start_sasl -eval "application:start(transcode), reloader:start()"


shell:
	erl -name tr_shell -setcookie `cat priv/cookie` -remsh transcode

tidy:
	erl -detached -eval "erl_tidy:dir(\"src\", [{recursive,true}, {verbose, true}])"
	find . -name "*.bak" -exec rm {} \;
