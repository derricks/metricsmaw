%%% =========================
%%% @doc An Http interface for use with metricsmaw. Adapted (along with supervisor) from Erlang and OTP In Action
%%% =========================
-module(http_interface).

-behaviour(gen_server).

-record(state,{lsock,socket,request_line,headers = [],body = <<>>,content_remaining=0,parent}).

%%% --------------------
%%% API
%%% --------------------
-export([start_link/1]).

%%% gen_server
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

start_link(LSock) ->
	gen_server:start_link(?MODULE,[LSock,self()],[]).


%%% gen_server callbacks
init([LSock,Parent]) ->
	State = #state{lsock=LSock,parent=Parent},
	{ok,State,0}.
	
handle_call(_Request,_From,State) -> {reply,ok,State}.

handle_cast(_Request,State) -> {noreply,State}.

% first line
handle_info({http,_Sock,{http_request,_,_,_}=Request},State) ->
	inet:setopts(State#state.socket,[{active,once}]),
	{noreply,State#state{request_line=Request}};
%header
handle_info({http,_Sock,{http_header,_,Name,_,Value}},State) ->
	inet:setopts(State#state.socket,[{active,once}]),
	{noreply,header(Name,Value,State)};
% end of headers with no more content
handle_info({http,_Sock,http_eoh},#state{content_remaining=0}=State) ->
	{stop,normal,handle_http_request(State)};
% end of headers with content remaining
handle_info({http,_Sock,http_eoh},State) ->
	inet:setopts(State#state.socket,[{active,once},{packet,raw}]),
	{noreply,State};
handle_info({tcp,_Sock,Data},State) when is_binary(Data) ->
	ContentRem = State#state.content_remaining - byte_size(Data),
	Body = list_to_binary([State#state.body,Data]),
	NewState = State#state{body=Body,content_remaining=ContentRem},
	if ContentRem > 0 ->
		inet:setopts(State#state.socket,[{active,once}]),
		{noreply,NewState};
		true -> {stop,normal,handle_http_request(NewState)}
	end;
handle_info({tcp_closed,_Sock},State) ->
	{stop,normal,State};
handle_info(timeout,#state{lsock=LSock,parent=Parent} = State) ->
	{ok,Socket} = gen_tcp:accept(LSock),
	hi_sup:start_child(Parent),
	inet:setopts(Socket,[{active,once}]),
	{noreply,State#state{socket=Socket}}.
	
terminate(_Reason,_State) -> ok.

code_change(_OldVersion,State,_Extra) ->
	{ok,State}.
	
%%% --------------------------
%%% privates
%%% --------------------------
header('Content-Length'=Name,Value,State) ->
	ContentLength = list_to_integer(binary_to_list(Value)),
	State#state{content_remaining=ContentLength,headers=[{Name,Value}|State#state.headers]};
header(Name,Value,State) ->
	State#state{headers=[{Name,Value}|State#state.headers]}.
	
%%% ---------------------
%%% Handles (including sending response down the socket) incoming requests once they've been parsed into state records
%%%----------------------
handle_http_request(State) ->
	#state{request_line={http_request,Method,_Uri,_Version}} = State,
	gen_tcp:send(State#state.socket,handle_http_request(Method,State)).
		
%%% ----------------------
%%% @doc Specific request handling
%%% -----------------------
% Get a specific metric's current value
handle_http_request('GET',State) ->
	#state{request_line={http_request,_Method,Uri,_Version}} = State,	
	{MetricName,MetricType} = parse_metric_info(Uri),
	response_with_metric(metrics:get_data(MetricName,MetricType));
% Update a specific metric's current value
handle_http_request('POST',State) ->
	#state{request_line={http_request,_Method,Uri,_Version},body=Body} = State,
	metrics:add_data('metricsmaw.system.http.post-body.data_rate',meter_minute,byte_size(Body)),	
	[BodyText|_] = string:tokens(binary_to_list(Body),"\n"),
	{MetricName,MetricType} = parse_metric_info(Uri),
	Value = try list_to_float(BodyText) of
		        ParsedFloat -> ParsedFloat
		    catch
			    % try to parse as int, otherwise let exception propagate
				_:_ -> list_to_integer(BodyText)
			end,
			     
	metrics:add_data(MetricName,MetricType,Value),	
	response_with_metric(Value).
	
%% -----------------
%% @docs parse the metric info out of a URI
%% -----------------
parse_metric_info({abs_path,MetricsUri}) ->
	["metrics",MetricName,MetricType|_Rest] = string:tokens(binary_to_list(MetricsUri),"/"),
	{list_to_atom(MetricName),list_to_atom(MetricType)}.
	
%%% ---------------
%%% convenience functions for creating response strings off of metrics
%%% --------------
response_with_metric(MetricValue) when is_number(MetricValue) ->
	http_reply(200,[],io_lib:format("~w",[MetricValue]));
response_with_metric(undefined) ->
	http_reply(404);
response_with_metric(MetricValue) when is_tuple(MetricValue) ->
	http_reply(200,[],tuple_to_list(MetricValue)).
	
		
http_reply(Code,Headers,Body) ->
	ContentBytes = iolist_to_binary(Body),
	Length = byte_size(ContentBytes),
	[io_lib:format("HTTP/1.1 ~s\r\n~sContent-Length: ~w\r\n\r\n",[response(Code),headers(Headers),Length]),ContentBytes].
	
http_reply(Code) ->
	io_lib:format("HTTP/1.1 ~s\r\n",[Code]).
	
response(200) -> "200 OK";
response(100) -> "100 Continue";
response(404) -> "404 Not Found";
response(501) -> "501 Not implemented";
response(Code) -> integer_to_list(Code).

headers([{Name,Value}|T]) ->
	[io_lib:format("~s: ~s\r\n",[Name,Value]) | headers(T)];
headers([]) -> [].
	
	
	




