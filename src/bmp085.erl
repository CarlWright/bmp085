
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
%% related to the sensor. This is data extracted from the sensor hardware
%% we are using.
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
    read_pressure/2,
    read_altitude/2]).


-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(BMP_CONTROL, 16#F4).
-define(BMP_TEMP, 16#2E).
-define(BMP_PRESSURE, 16#34).
-define(BMP_PRESSURE_LOC, 16#F6).

-define(ULTRALOWPOWER, 16#0).     %% ultra low power mode 
-define(STANDARD, 16#1).          %% standard mode 
-define(HIGHRES, 16#2).           %% high resultion mode 
-define(ULTRAHIGHRES, 16#3).       %% ultra high resultion mode 

-define(ULTRALOW_DELAY,5).
-define(STANDARD_DELAY, 8).
-define(HIGHRES_DELAY, 14).
-define(ULTRAHIGHRES_DELAY, 26).
-define(SEALEVEL_PA,  101325.0).

-ifdef(debug).
-define(LOG(X,Y), io:format(X,Y)).
-else.
-define(LOG(X,Y), true).
-endif.

%%%===================================================================
%%% API
%%%===================================================================

read_temp(PID) ->
    gen_server:call(PID,{temperature}).

read_pressure(PID, ultralowpower) ->
    gen_server:call(PID,{pressure, ultralowpower});
read_pressure(PID, standard) ->
    gen_server:call(PID,{pressure, standard});
read_pressure(PID, highres) ->
    gen_server:call(PID,{pressure, highres});
read_pressure(PID, ultrahighres) ->
    gen_server:call(PID,{pressure, ultrahighres}).

read_altitude(PID, ultralowpower) ->
    gen_server:call(PID,{altitude, ultralowpower});
read_altitude(PID, standard) ->
    gen_server:call(PID,{altitude, standard});
read_altitude(PID, highres) ->
    gen_server:call(PID,{altitude, highres});
read_altitude(PID, ultrahighres) ->
    gen_server:call(PID,{altitude, ultrahighres}).



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
handle_call({pressure, ultralowpower}, _From, State) ->
    Pressure = pressure_read(State,?ULTRALOWPOWER,?ULTRALOW_DELAY),
    {reply, {ok, Pressure}, State};
handle_call({pressure, standard}, _From, State) ->
    Pressure = pressure_read(State,?STANDARD,?STANDARD_DELAY),
    {reply, {ok, Pressure}, State};
handle_call({pressure, highres}, _From, State) ->
    Pressure = pressure_read(State,?HIGHRES,?HIGHRES_DELAY),
    {reply, {ok, Pressure}, State};
handle_call({pressure, ultrahighres}, _From, State) ->
    Pressure = pressure_read(State,?ULTRAHIGHRES,?ULTRAHIGHRES_DELAY),
    {reply, {ok, Pressure}, State};

handle_call({altitude, ultralowpower}, _From, State) ->
    Altitude = altitude_read(State,?ULTRALOWPOWER,?ULTRALOW_DELAY),
    {reply, {ok, Altitude}, State};
handle_call({altitude, standard}, _From, State) ->
    Altitude = altitude_read(State,?STANDARD,?STANDARD_DELAY),
    {reply, {ok, Altitude}, State};
handle_call({altitude, highres}, _From, State) ->
    Altitude = altitude_read(State,?HIGHRES,?HIGHRES_DELAY),
    {reply, {ok, Altitude}, State};
handle_call({altitude, ultrahighres}, _From, State) ->
    Altitude = altitude_read(State,?ULTRAHIGHRES,?ULTRAHIGHRES_DELAY),
    {reply, {ok, Altitude}, State};

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
    
read_raw_pressure(Sensor, Mode, Delay) ->
    i2c:write(Sensor, << ?BMP_CONTROL, (?BMP_PRESSURE + (Mode bsl 6)) >>),
    timer:sleep(Delay),
    Result = i2c:write_read(Sensor,<<?BMP_PRESSURE_LOC>>,4),
    ?LOG("Raw pressure result = ~w~n",[Result]),
    << MSB, LSB, _, RB>> = Result,
    ?LOG("MSB = ~.16#, LSB = ~.16#, RB = ~.16#~n",[MSB, LSB, RB]), 
    pressure_convert(MSB, LSB, RB, Mode).
    

round(Number, Precision) ->
    P = math:pow(10, Precision),
    round(Number * P) / P.

pressure_convert(A, B, C, Mode) -> 
    ?LOG("A = ~.16#, B = ~.16#, C = ~.16#~n",[A,B,C]), 
    ((A bsl 16 ) + (B bsl 8) + C) bsr (8 - Mode).


%% This is a complex step in the pressure calculation that was best taken
%% out into a function with a guard expression
p_calc(B7, B4) when B7 < 16#80000000 ->
    (B7 * 2 ) / B4;
p_calc(B7, B4) ->
    (B7 / B4) * 2.

pressure_read(State, Mode, Delay) ->
    UT = read_raw_temp(State#state.sensor_pid),
    UP = read_raw_pressure(State#state.sensor_pid, Mode, Delay),
    ?LOG("UP = ~w~n",[UP]),

    X1 = ((UT - State#state.ac6) * State#state.ac5) bsr 15,
    X2 = (State#state.mc bsl 11) / (X1 + State#state.mdval),
    B5 = trunc(X1 + X2),
    ?LOG("B5 = ~w~n",[B5]),

    B6 = B5 - 4000,
    ?LOG("B6 = ~w~n",[B6]),

    T1 = (State#state.b2 * (trunc(B6 * B6) bsr 12)) bsr 11,
    T2 = (State#state.ac2 * B6) bsr 11,
    T3 = T1 + T2,
    B3 = (((State#state.ac1 * 4 + T3) bsl Mode) + 2) / 4,
    ?LOG("B3 = ~w~n",[B3]),

    W1 = (State#state.ac3 * B6) bsr 13,
    W2 = (State#state.b1 * (trunc(B6 * B6) bsr 12)) bsr 16,
    W3 = ((W1 + W2) + 2) bsr 2,
    B4 = (State#state.ac4 * (W3 + 32768)) bsr 15,
    ?LOG("B4 = ~w~n",[B4]),

    B7 = trunc((UP - B3) * (50000 bsr Mode)),
    ?LOG("B7 = ~w~n",[B7]),
    P = trunc(p_calc(B7, B4)),
    Z1 = (P bsr 8) * (P bsr 8),
    Z2 = (Z1 * 3038) bsr 16,
    Z3 = (-7357 * P) bsr 16,
    Result = P + ((Z2 + Z3 + 3791) bsr 4),
    Result.

altitude_read(State, Mode, Delay) ->
Pressure = float(pressure_read(State, Mode, Delay) ),
44330.0 * ( 1.0 - math:pow(( Pressure / ?SEALEVEL_PA), (1.0 /5.255))) .
