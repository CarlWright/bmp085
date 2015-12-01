
%%%-------------------------------------------------------------------
%%% @author Carl A. Wright <wright@servicelevel.net>
%%% @copyright (C) 2015, Carl A. Wright
%%% @doc
%%%
%%% @end
%%% Created : 23 Nov 2015 
%%%-------------------------------------------------------------------
-module(bmp085).

-behaviour(gen_server).


%% The PID for the sensor process and the calibration data
%% related to the sensor.
-record( state, {sensor_pid,
		 ac1,
		 ac2,
		 ac3,
		 ac4,
		 ac5,
		 ac6,
		 b1,
		 b2,
		 mb,
		 mc,
		 mdval}).



%% API
-export(
   [read_temp/1,
    read_pressure/1,
    read_altitude/1]).


-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(BMP_CONTROL, 16#F4).
-define(BMP_TEMP, 16#2E).
-define(BMP_PRESSURE, 16#34).
-define(BMP_PRESSURE_LOC, 16#F6).
-define(MODE, 16#1).       %% standard mode (3 others exist)
%%%===================================================================
%%% API
%%%===================================================================

read_temp(PID) ->
    gen_server:call(PID,{temperature}).

read_pressure(PID) ->
    gen_server:call(PID,{pressure}).

read_altitude(_PID) ->
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, Sensor} = i2c:start_link("i2c-1",16#77),

    AC1 = download_register(Sensor, 16#AA),
    AC2 = download_register(Sensor, 16#AC),
    AC3 = download_register(Sensor, 16#AE),
    AC4 = download_unsigned_register(Sensor, 16#B0),
    AC5 = download_unsigned_register(Sensor, 16#B2),
    AC6 = download_unsigned_register(Sensor, 16#B4),
    B1 = download_register(Sensor, 16#B6),
    B2 = download_register(Sensor, 16#B8),
    MB = download_register(Sensor, 16#BA),
    MC = download_register(Sensor, 16#BC),
    MDVAL = download_register(Sensor, 16#BE),
    {ok, #state{sensor_pid = Sensor,
		ac1 = AC1, 
		ac2 = AC2, 
		ac3 = AC3, 
		ac4 = AC4, 
		ac5 = AC5, 
		ac6 = AC6, 
		b1 = B1, 
		b2 = B2, 
		mb = MB, 
		mc = MC, 
		mdval = MDVAL}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({temperature}, _From, State) ->
    UT = read_raw_temp(State#state.sensor_pid),
    X1 = ((UT - State#state.ac6) * State#state.ac5) bsr 15,
    X2 = (State#state.mc bsl 11) / (X1 + State#state.mdval),
    Celsius = (X1 + X2 + 8) / 160,
    Reply = {ok, round(Celsius,2),
	     round( (Celsius * 1.8) + 32, 2)},
    {reply, Reply, State};
handle_call({pressure}, _From, State) ->
    UT = read_raw_temp(State#state.sensor_pid),
    UP = read_raw_pressure(State#state.sensor_pid),
    io:format("UP = ~w~n",[UP]),
    Mode = 3,

    X1 = ((UT - State#state.ac6) * State#state.ac5) bsr 15,
    X2 = (State#state.mc bsl 11) / (X1 + State#state.mdval),
    B5 = trunc(X1 + X2),
    io:format("B5 = ~w~n",[B5]),

    B6 = B5 - 4000,
    io:format("B6 = ~w~n",[B6]),

    T1 = (State#state.b2 * (trunc(B6 * B6) bsr 12)) bsr 11,
    T2 = (State#state.ac2 * B6) bsr 11,
    T3 = T1 + T2,
    B3 = (((State#state.ac1 * 4 + T3) bsl Mode) + 2) / 4,
    io:format("B3 = ~w~n",[B3]),

    W1 = (State#state.ac3 * B6) bsr 13,
    W2 = (State#state.b1 * (trunc(B6 * B6) bsr 12)) bsr 16,
    W3 = ((W1 + W2) + 2) bsr 2,
    B4 = (State#state.ac4 * (W3 + 32768)) bsr 15,
    io:format("B4 = ~w~n",[B4]),

    B7 = trunc((UP - B3) * (50000 bsr Mode)),
    io:format("B7 = ~w~n",[B7]),
    P = trunc(p_calc(B7, B4)),
    Z1 = (P bsr 8) * (P bsr 8),
    Z2 = (Z1 * 3038) bsr 16,
    Z3 = (-7357 * P) bsr 16,
    Pressure = P + ((Z2 + Z3 + 3791) bsr 4),

    Reply = {ok, Pressure},

    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% the register's hold calibration numbers for the specific
%% sensor needed to convert raw data into conventional measurements
download_register( Sensor, Address) ->
    << N1, N2 >> = i2c:write_read(Sensor,<< Address >>, 2),
    register_convert(N1, N2).

download_unsigned_register( Sensor, Address) ->
    << N1, N2 >> = i2c:write_read(Sensor,<< Address >>, 2),
    (N1 * 256) + N2.


%% The register results received are two eight bit bytes
%% representing a 2's complement integer
register_convert(N1, N2) when N1 > 16#7F ->
    - ((255 - N1) bsl 8 + (255 - N2) + 1);
register_convert( N1, N2) ->
    (N1 * 256) + N2.


read_raw_temp(Sensor) ->
    i2c:write(Sensor, << ?BMP_CONTROL,?BMP_TEMP >>),
    timer:sleep(5),
    <<_, _, MSB, LSB, _/binary>> = i2c:read(Sensor,8),
    register_convert(MSB, LSB).
    
read_raw_pressure(Sensor) ->
    i2c:write(Sensor, << ?BMP_CONTROL,?BMP_PRESSURE >>),
    timer:sleep(8),
    Result = i2c:write_read(Sensor,<<?BMP_PRESSURE_LOC>>,4),
    io:format("Raw pressure result = ~w~n",[Result]),
    << MSB, LSB, _, RB>> = Result,
    io:format("MSB = ~.16#, LSB = ~.16#, RB = ~.16#~n",[MSB, LSB, RB]), 
    pressure_convert(MSB, LSB, RB).
    

round(Number, Precision) ->
    P = math:pow(10, Precision),
    round(Number * P) / P.

pressure_convert(A, B, C) -> 
    io:format("A = ~.16#, B = ~.16#, C = ~.16#~n",[A,B,C]), 
    ((A bsl 16) + (B bsl 8) + C) bsr 5.

p_calc(B7, B4) when B7 < 16#80000000 ->
    (B7 * 2 ) / B4;
p_calc(B7, B4) ->
    (B7 / B4) * 2.
