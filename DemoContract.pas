unit DemoContract;

// auto generated source at 04.09.2017 20:33:54

interface

uses System.Types, System.SysUtils, Generics.Collections, Rest.Json,
  DEthereum, DEthereum.Types;

type
  TEth_ContractDemoContract = class(TEthereumContract)
  private
    procedure InitContract;
  public
    constructor Create;
    function type_bytes32(const gas, gasPrice: Int64; out __type_bytes32: TByteDynArray): Boolean;
    function set_int32(const gas, gasPrice: Int64; const value: Integer; out __set_int32: Integer): Boolean;
    function set_bytes32(const gas, gasPrice: Int64; const value: TByteDynArray; out __set_bytes32: TByteDynArray): Boolean;
    function set_int64(const gas, gasPrice: Int64; const value: Int64; out __set_int64: Int64): Boolean;
    function type_bool(const gas, gasPrice: Int64; out __type_bool: Boolean): Boolean;
    function set_bool(const gas, gasPrice: Int64; const value: Boolean; out __set_bool: Boolean): Boolean;
    function totalEvents(const gas, gasPrice: Int64; out __totalEvents: Int64): Boolean;
    function type_int32(const gas, gasPrice: Int64; out __type_int32: Integer): Boolean;
    function set_string(const gas, gasPrice: Int64; const value: String; out __set_string: String): Boolean;
    function type_string(const gas, gasPrice: Int64; out __type_string: String): Boolean;
    function type_int64(const gas, gasPrice: Int64; out __type_int64: Int64): Boolean;
    function FilterEvent_type_int32(const FromBlockNumber: TEth_BlockNumber; const FromBlockNumberCustom: Int64; const ToBlockNumber: TEth_BlockNumber; const ToBlockNumberCustom: Int64): Boolean;
    function GetEvent_type_int32(const Index: Int64; out value: Integer): Boolean;
    function FilterEvent_type_int64(const FromBlockNumber: TEth_BlockNumber; const FromBlockNumberCustom: Int64; const ToBlockNumber: TEth_BlockNumber; const ToBlockNumberCustom: Int64): Boolean;
    function GetEvent_type_int64(const Index: Int64; out value: Int64): Boolean;
    function FilterEvent_type_bool(const FromBlockNumber: TEth_BlockNumber; const FromBlockNumberCustom: Int64; const ToBlockNumber: TEth_BlockNumber; const ToBlockNumberCustom: Int64): Boolean;
    function GetEvent_type_bool(const Index: Int64; out value: Boolean): Boolean;
    function FilterEvent_type_string(const FromBlockNumber: TEth_BlockNumber; const FromBlockNumberCustom: Int64; const ToBlockNumber: TEth_BlockNumber; const ToBlockNumberCustom: Int64): Boolean;
    function GetEvent_type_string(const Index: Int64; out value: String): Boolean;
    function FilterEvent_type_bytes32(const FromBlockNumber: TEth_BlockNumber; const FromBlockNumberCustom: Int64; const ToBlockNumber: TEth_BlockNumber; const ToBlockNumberCustom: Int64): Boolean;
    function GetEvent_type_bytes32(const Index: Int64; out value: TByteDynArray): Boolean;
  end;

implementation

constructor TEth_ContractDemoContract.Create;
begin
  inherited;
  InitContract;
  ContractAddress := '0x655692176986143bc1a6b793985193fde450b007';
end;

procedure TEth_ContractDemoContract.InitContract;
var
  Event: TEthereumContractEvent;
  Method: TEthereumContractMethod;
  Parameter: TEthereumContractParameter;
begin

  Method := TEthereumContractMethod.Create;
  Method.MethodName := 'type_bytes32';
  Method.MethodType := 'function';
  Method.MethodConstant := True;
  Method.MethodHash := '055825de';

  Parameter := TEthereumContractParameter.Create;
  Parameter.Name := '__type_bytes32';
  Parameter.&Type := 'bytes32';
  Method.Outputs.Add(Parameter);
  Methods.Add(Method);

  Method := TEthereumContractMethod.Create;
  Method.MethodName := 'set_int32';
  Method.MethodType := 'function';
  Method.MethodConstant := False;
  Method.MethodHash := '1613447b';

  Parameter := TEthereumContractParameter.Create;
  Parameter.Name := 'value';
  Parameter.&Type := 'int32';
  Method.Inputs.Add(Parameter);

  Parameter := TEthereumContractParameter.Create;
  Parameter.Name := '__set_int32';
  Parameter.&Type := 'int32';
  Method.Outputs.Add(Parameter);
  Methods.Add(Method);

  Method := TEthereumContractMethod.Create;
  Method.MethodName := 'set_bytes32';
  Method.MethodType := 'function';
  Method.MethodConstant := False;
  Method.MethodHash := '7bf5cddc';

  Parameter := TEthereumContractParameter.Create;
  Parameter.Name := 'value';
  Parameter.&Type := 'bytes32';
  Method.Inputs.Add(Parameter);

  Parameter := TEthereumContractParameter.Create;
  Parameter.Name := '__set_bytes32';
  Parameter.&Type := 'bytes32';
  Method.Outputs.Add(Parameter);
  Methods.Add(Method);

  Method := TEthereumContractMethod.Create;
  Method.MethodName := 'set_int64';
  Method.MethodType := 'function';
  Method.MethodConstant := False;
  Method.MethodHash := '94310ea2';

  Parameter := TEthereumContractParameter.Create;
  Parameter.Name := 'value';
  Parameter.&Type := 'int64';
  Method.Inputs.Add(Parameter);

  Parameter := TEthereumContractParameter.Create;
  Parameter.Name := '__set_int64';
  Parameter.&Type := 'int64';
  Method.Outputs.Add(Parameter);
  Methods.Add(Method);

  Method := TEthereumContractMethod.Create;
  Method.MethodName := 'type_bool';
  Method.MethodType := 'function';
  Method.MethodConstant := True;
  Method.MethodHash := '97a4220d';

  Parameter := TEthereumContractParameter.Create;
  Parameter.Name := '__type_bool';
  Parameter.&Type := 'bool';
  Method.Outputs.Add(Parameter);
  Methods.Add(Method);

  Method := TEthereumContractMethod.Create;
  Method.MethodName := 'set_bool';
  Method.MethodType := 'function';
  Method.MethodConstant := False;
  Method.MethodHash := 'a66162a6';

  Parameter := TEthereumContractParameter.Create;
  Parameter.Name := 'value';
  Parameter.&Type := 'bool';
  Method.Inputs.Add(Parameter);

  Parameter := TEthereumContractParameter.Create;
  Parameter.Name := '__set_bool';
  Parameter.&Type := 'bool';
  Method.Outputs.Add(Parameter);
  Methods.Add(Method);

  Method := TEthereumContractMethod.Create;
  Method.MethodName := 'totalEvents';
  Method.MethodType := 'function';
  Method.MethodConstant := True;
  Method.MethodHash := 'ba870686';

  Parameter := TEthereumContractParameter.Create;
  Parameter.Name := '__totalEvents';
  Parameter.&Type := 'int64';
  Method.Outputs.Add(Parameter);
  Methods.Add(Method);

  Method := TEthereumContractMethod.Create;
  Method.MethodName := 'type_int32';
  Method.MethodType := 'function';
  Method.MethodConstant := True;
  Method.MethodHash := 'c7d48022';

  Parameter := TEthereumContractParameter.Create;
  Parameter.Name := '__type_int32';
  Parameter.&Type := 'int32';
  Method.Outputs.Add(Parameter);
  Methods.Add(Method);

  Method := TEthereumContractMethod.Create;
  Method.MethodName := 'set_string';
  Method.MethodType := 'function';
  Method.MethodConstant := False;
  Method.MethodHash := 'c9615770';

  Parameter := TEthereumContractParameter.Create;
  Parameter.Name := 'value';
  Parameter.&Type := 'string';
  Method.Inputs.Add(Parameter);

  Parameter := TEthereumContractParameter.Create;
  Parameter.Name := '__set_string';
  Parameter.&Type := 'string';
  Method.Outputs.Add(Parameter);
  Methods.Add(Method);

  Method := TEthereumContractMethod.Create;
  Method.MethodName := 'type_string';
  Method.MethodType := 'function';
  Method.MethodConstant := True;
  Method.MethodHash := 'f8e19c61';

  Parameter := TEthereumContractParameter.Create;
  Parameter.Name := '__type_string';
  Parameter.&Type := 'string';
  Method.Outputs.Add(Parameter);
  Methods.Add(Method);

  Method := TEthereumContractMethod.Create;
  Method.MethodName := 'type_int64';
  Method.MethodType := 'function';
  Method.MethodConstant := True;
  Method.MethodHash := 'f952143e';

  Parameter := TEthereumContractParameter.Create;
  Parameter.Name := '__type_int64';
  Parameter.&Type := 'int64';
  Method.Outputs.Add(Parameter);
  Methods.Add(Method);

  Event := TEthereumContractEvent.Create;
  Event.EventName := 'type_int32';
  Event.EventType := 'event';
  Event.EventHash := '';
  Event.EventAnonymous := False;

  Parameter := TEthereumContractParameter.Create;
  Parameter.Name := 'value';
  Parameter.&Type := 'int32';
  Parameter.Indexed := False;
  Event.Parameters.Add(Parameter);
  Events.Add(Event);

  Event := TEthereumContractEvent.Create;
  Event.EventName := 'type_int64';
  Event.EventType := 'event';
  Event.EventHash := '';
  Event.EventAnonymous := False;

  Parameter := TEthereumContractParameter.Create;
  Parameter.Name := 'value';
  Parameter.&Type := 'int64';
  Parameter.Indexed := False;
  Event.Parameters.Add(Parameter);
  Events.Add(Event);

  Event := TEthereumContractEvent.Create;
  Event.EventName := 'type_bool';
  Event.EventType := 'event';
  Event.EventHash := '';
  Event.EventAnonymous := False;

  Parameter := TEthereumContractParameter.Create;
  Parameter.Name := 'value';
  Parameter.&Type := 'bool';
  Parameter.Indexed := False;
  Event.Parameters.Add(Parameter);
  Events.Add(Event);

  Event := TEthereumContractEvent.Create;
  Event.EventName := 'type_string';
  Event.EventType := 'event';
  Event.EventHash := '';
  Event.EventAnonymous := False;

  Parameter := TEthereumContractParameter.Create;
  Parameter.Name := 'value';
  Parameter.&Type := 'string';
  Parameter.Indexed := False;
  Event.Parameters.Add(Parameter);
  Events.Add(Event);

  Event := TEthereumContractEvent.Create;
  Event.EventName := 'type_bytes32';
  Event.EventType := 'event';
  Event.EventHash := '';
  Event.EventAnonymous := False;

  Parameter := TEthereumContractParameter.Create;
  Parameter.Name := 'value';
  Parameter.&Type := 'bytes32';
  Parameter.Indexed := False;
  Event.Parameters.Add(Parameter);
  Events.Add(Event);
end;

function TEth_ContractDemoContract.type_bytes32(const gas, gasPrice: Int64; out __type_bytes32: TByteDynArray): Boolean;
var
  Code, CallResult: String;
  Method: TEthereumContractMethod;
begin
  Method := GetMethod('type_bytes32');
  if Assigned(Method) then
    if Method.MethodConstant
      then Result := BuildCallCode(Method, [], Code) and eth_call(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, ethbnLatest, 0, CallResult)
      else Result := BuildCallCode(Method, [], Code) and personal_signAndSendTransaction(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, CoinPassword, CallResult)
  else
  begin
    Result := ErrorNotFound('type_bytes32');
    Exit;
  end;
  if Result and (Method.Outputs.Count > 0) then
    begin
      Result := ParseCallCode(Method.MethodName, CallResult, Method.Outputs);
      if Result then
        try
          __type_bytes32 := Method.Outputs[0].AsBytes;
        except
        on E: Exception do
          Result := ErrorParamConvert(Method.MethodName, E);
        end
    end;
end;

function TEth_ContractDemoContract.set_int32(const gas, gasPrice: Int64; const value: Integer; out __set_int32: Integer): Boolean;
var
  Code, CallResult: String;
  Method: TEthereumContractMethod;
begin
  Method := GetMethod('set_int32');
  if Assigned(Method) then
    if Method.MethodConstant
      then Result := BuildCallCode(Method, [value], Code) and eth_call(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, ethbnLatest, 0, CallResult)
      else Result := BuildCallCode(Method, [value], Code) and personal_signAndSendTransaction(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, CoinPassword, CallResult)
  else
  begin
    Result := ErrorNotFound('set_int32');
    Exit;
  end;
  if Result and (Method.Outputs.Count > 0) then
    begin
      Result := ParseCallCode(Method.MethodName, CallResult, Method.Outputs);
      if Result then
        try
          __set_int32 := Method.Outputs[0].AsInteger;
        except
        on E: Exception do
          Result := ErrorParamConvert(Method.MethodName, E);
        end
    end;
end;

function TEth_ContractDemoContract.set_bytes32(const gas, gasPrice: Int64; const value: TByteDynArray; out __set_bytes32: TByteDynArray): Boolean;
var
  Code, CallResult: String;
  Method: TEthereumContractMethod;
begin
  Method := GetMethod('set_bytes32');
  if Assigned(Method) then
    if Method.MethodConstant
      then Result := BuildCallCode(Method, [eth_bytesToStr(value)], Code) and eth_call(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, ethbnLatest, 0, CallResult)
      else Result := BuildCallCode(Method, [eth_bytesToStr(value)], Code) and personal_signAndSendTransaction(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, CoinPassword, CallResult)
  else
  begin
    Result := ErrorNotFound('set_bytes32');
    Exit;
  end;
  if Result and (Method.Outputs.Count > 0) then
    begin
      Result := ParseCallCode(Method.MethodName, CallResult, Method.Outputs);
      if Result then
        try
          __set_bytes32 := Method.Outputs[0].AsBytes;
        except
        on E: Exception do
          Result := ErrorParamConvert(Method.MethodName, E);
        end
    end;
end;

function TEth_ContractDemoContract.set_int64(const gas, gasPrice: Int64; const value: Int64; out __set_int64: Int64): Boolean;
var
  Code, CallResult: String;
  Method: TEthereumContractMethod;
begin
  Method := GetMethod('set_int64');
  if Assigned(Method) then
    if Method.MethodConstant
      then Result := BuildCallCode(Method, [value], Code) and eth_call(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, ethbnLatest, 0, CallResult)
      else Result := BuildCallCode(Method, [value], Code) and personal_signAndSendTransaction(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, CoinPassword, CallResult)
  else
  begin
    Result := ErrorNotFound('set_int64');
    Exit;
  end;
  if Result and (Method.Outputs.Count > 0) then
    begin
      Result := ParseCallCode(Method.MethodName, CallResult, Method.Outputs);
      if Result then
        try
          __set_int64 := Method.Outputs[0].AsInt64;
        except
        on E: Exception do
          Result := ErrorParamConvert(Method.MethodName, E);
        end
    end;
end;

function TEth_ContractDemoContract.type_bool(const gas, gasPrice: Int64; out __type_bool: Boolean): Boolean;
var
  Code, CallResult: String;
  Method: TEthereumContractMethod;
begin
  Method := GetMethod('type_bool');
  if Assigned(Method) then
    if Method.MethodConstant
      then Result := BuildCallCode(Method, [], Code) and eth_call(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, ethbnLatest, 0, CallResult)
      else Result := BuildCallCode(Method, [], Code) and personal_signAndSendTransaction(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, CoinPassword, CallResult)
  else
  begin
    Result := ErrorNotFound('type_bool');
    Exit;
  end;
  if Result and (Method.Outputs.Count > 0) then
    begin
      Result := ParseCallCode(Method.MethodName, CallResult, Method.Outputs);
      if Result then
        try
          __type_bool := Method.Outputs[0].AsBoolean;
        except
        on E: Exception do
          Result := ErrorParamConvert(Method.MethodName, E);
        end
    end;
end;

function TEth_ContractDemoContract.set_bool(const gas, gasPrice: Int64; const value: Boolean; out __set_bool: Boolean): Boolean;
var
  Code, CallResult: String;
  Method: TEthereumContractMethod;
begin
  Method := GetMethod('set_bool');
  if Assigned(Method) then
    if Method.MethodConstant
      then Result := BuildCallCode(Method, [value], Code) and eth_call(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, ethbnLatest, 0, CallResult)
      else Result := BuildCallCode(Method, [value], Code) and personal_signAndSendTransaction(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, CoinPassword, CallResult)
  else
  begin
    Result := ErrorNotFound('set_bool');
    Exit;
  end;
  if Result and (Method.Outputs.Count > 0) then
    begin
      Result := ParseCallCode(Method.MethodName, CallResult, Method.Outputs);
      if Result then
        try
          __set_bool := Method.Outputs[0].AsBoolean;
        except
        on E: Exception do
          Result := ErrorParamConvert(Method.MethodName, E);
        end
    end;
end;

function TEth_ContractDemoContract.totalEvents(const gas, gasPrice: Int64; out __totalEvents: Int64): Boolean;
var
  Code, CallResult: String;
  Method: TEthereumContractMethod;
begin
  Method := GetMethod('totalEvents');
  if Assigned(Method) then
    if Method.MethodConstant
      then Result := BuildCallCode(Method, [], Code) and eth_call(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, ethbnLatest, 0, CallResult)
      else Result := BuildCallCode(Method, [], Code) and personal_signAndSendTransaction(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, CoinPassword, CallResult)
  else
  begin
    Result := ErrorNotFound('totalEvents');
    Exit;
  end;
  if Result and (Method.Outputs.Count > 0) then
    begin
      Result := ParseCallCode(Method.MethodName, CallResult, Method.Outputs);
      if Result then
        try
          __totalEvents := Method.Outputs[0].AsInt64;
        except
        on E: Exception do
          Result := ErrorParamConvert(Method.MethodName, E);
        end
    end;
end;

function TEth_ContractDemoContract.type_int32(const gas, gasPrice: Int64; out __type_int32: Integer): Boolean;
var
  Code, CallResult: String;
  Method: TEthereumContractMethod;
begin
  Method := GetMethod('type_int32');
  if Assigned(Method) then
    if Method.MethodConstant
      then Result := BuildCallCode(Method, [], Code) and eth_call(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, ethbnLatest, 0, CallResult)
      else Result := BuildCallCode(Method, [], Code) and personal_signAndSendTransaction(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, CoinPassword, CallResult)
  else
  begin
    Result := ErrorNotFound('type_int32');
    Exit;
  end;
  if Result and (Method.Outputs.Count > 0) then
    begin
      Result := ParseCallCode(Method.MethodName, CallResult, Method.Outputs);
      if Result then
        try
          __type_int32 := Method.Outputs[0].AsInteger;
        except
        on E: Exception do
          Result := ErrorParamConvert(Method.MethodName, E);
        end
    end;
end;

function TEth_ContractDemoContract.set_string(const gas, gasPrice: Int64; const value: String; out __set_string: String): Boolean;
var
  Code, CallResult: String;
  Method: TEthereumContractMethod;
begin
  Method := GetMethod('set_string');
  if Assigned(Method) then
    if Method.MethodConstant
      then Result := BuildCallCode(Method, [value], Code) and eth_call(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, ethbnLatest, 0, CallResult)
      else Result := BuildCallCode(Method, [value], Code) and personal_signAndSendTransaction(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, CoinPassword, CallResult)
  else
  begin
    Result := ErrorNotFound('set_string');
    Exit;
  end;
  if Result and (Method.Outputs.Count > 0) then
    begin
      Result := ParseCallCode(Method.MethodName, CallResult, Method.Outputs);
      if Result then
        try
          __set_string := Method.Outputs[0].AsString;
        except
        on E: Exception do
          Result := ErrorParamConvert(Method.MethodName, E);
        end
    end;
end;

function TEth_ContractDemoContract.type_string(const gas, gasPrice: Int64; out __type_string: String): Boolean;
var
  Code, CallResult: String;
  Method: TEthereumContractMethod;
begin
  Method := GetMethod('type_string');
  if Assigned(Method) then
    if Method.MethodConstant
      then Result := BuildCallCode(Method, [], Code) and eth_call(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, ethbnLatest, 0, CallResult)
      else Result := BuildCallCode(Method, [], Code) and personal_signAndSendTransaction(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, CoinPassword, CallResult)
  else
  begin
    Result := ErrorNotFound('type_string');
    Exit;
  end;
  if Result and (Method.Outputs.Count > 0) then
    begin
      Result := ParseCallCode(Method.MethodName, CallResult, Method.Outputs);
      if Result then
        try
          __type_string := Method.Outputs[0].AsString;
        except
        on E: Exception do
          Result := ErrorParamConvert(Method.MethodName, E);
        end
    end;
end;

function TEth_ContractDemoContract.type_int64(const gas, gasPrice: Int64; out __type_int64: Int64): Boolean;
var
  Code, CallResult: String;
  Method: TEthereumContractMethod;
begin
  Method := GetMethod('type_int64');
  if Assigned(Method) then
    if Method.MethodConstant
      then Result := BuildCallCode(Method, [], Code) and eth_call(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, ethbnLatest, 0, CallResult)
      else Result := BuildCallCode(Method, [], Code) and personal_signAndSendTransaction(CoinAddress, ContractAddress, gas, gasPrice, 0, Code, CoinPassword, CallResult)
  else
  begin
    Result := ErrorNotFound('type_int64');
    Exit;
  end;
  if Result and (Method.Outputs.Count > 0) then
    begin
      Result := ParseCallCode(Method.MethodName, CallResult, Method.Outputs);
      if Result then
        try
          __type_int64 := Method.Outputs[0].AsInt64;
        except
        on E: Exception do
          Result := ErrorParamConvert(Method.MethodName, E);
        end
    end;
end;

function TEth_ContractDemoContract.FilterEvent_type_int32(const FromBlockNumber: TEth_BlockNumber; const FromBlockNumberCustom: Int64; const ToBlockNumber: TEth_BlockNumber; const ToBlockNumberCustom: Int64): Boolean;
var
  Topics: TArray<String>;
  Event: TEthereumContractEvent;
begin
  Event := GetEvent('type_int32');
  if Assigned(Event) then
    begin
      Result := GetEventHash(Event);
      if Result then
        begin
          SetLength(Topics, 1);
          Topics[0] := Event.EventHash;
          Event.Events.Clear;
          Result := eth_getLogs(FromBlockNumber, FromBlockNumberCustom, ToBlockNumber, ToBlockNumberCustom, ContractAddress, Topics, Event.Events);
        end;
    end else
    Result := ErrorNotFound('type_int32');
end;

function TEth_ContractDemoContract.GetEvent_type_int32(const Index: Int64; out value: Integer): Boolean;
var
  Event: TEthereumContractEvent;
begin
  Event := GetEvent('type_int32');
  if Assigned(Event) then
    try
      Result := ParseCallCode(Event.EventName, Event.Events[Index].Data, Event.Parameters);
      if Result then
        begin
          value := Event.Parameters[0].AsInteger;
        end;
    except
      on E: Exception do
        Result := ErrorParamConvert(Event.EventName, E);
    end else
    Result := ErrorNotFound('type_int32');
end;


function TEth_ContractDemoContract.FilterEvent_type_int64(const FromBlockNumber: TEth_BlockNumber; const FromBlockNumberCustom: Int64; const ToBlockNumber: TEth_BlockNumber; const ToBlockNumberCustom: Int64): Boolean;
var
  Topics: TArray<String>;
  Event: TEthereumContractEvent;
begin
  Event := GetEvent('type_int64');
  if Assigned(Event) then
    begin
      Result := GetEventHash(Event);
      if Result then
        begin
          SetLength(Topics, 1);
          Topics[0] := Event.EventHash;
          Event.Events.Clear;
          Result := eth_getLogs(FromBlockNumber, FromBlockNumberCustom, ToBlockNumber, ToBlockNumberCustom, ContractAddress, Topics, Event.Events);
        end;
    end else
    Result := ErrorNotFound('type_int64');
end;

function TEth_ContractDemoContract.GetEvent_type_int64(const Index: Int64; out value: Int64): Boolean;
var
  Event: TEthereumContractEvent;
begin
  Event := GetEvent('type_int64');
  if Assigned(Event) then
    try
      Result := ParseCallCode(Event.EventName, Event.Events[Index].Data, Event.Parameters);
      if Result then
        begin
          value := Event.Parameters[0].AsInt64;
        end;
    except
      on E: Exception do
        Result := ErrorParamConvert(Event.EventName, E);
    end else
    Result := ErrorNotFound('type_int64');
end;


function TEth_ContractDemoContract.FilterEvent_type_bool(const FromBlockNumber: TEth_BlockNumber; const FromBlockNumberCustom: Int64; const ToBlockNumber: TEth_BlockNumber; const ToBlockNumberCustom: Int64): Boolean;
var
  Topics: TArray<String>;
  Event: TEthereumContractEvent;
begin
  Event := GetEvent('type_bool');
  if Assigned(Event) then
    begin
      Result := GetEventHash(Event);
      if Result then
        begin
          SetLength(Topics, 1);
          Topics[0] := Event.EventHash;
          Event.Events.Clear;
          Result := eth_getLogs(FromBlockNumber, FromBlockNumberCustom, ToBlockNumber, ToBlockNumberCustom, ContractAddress, Topics, Event.Events);
        end;
    end else
    Result := ErrorNotFound('type_bool');
end;

function TEth_ContractDemoContract.GetEvent_type_bool(const Index: Int64; out value: Boolean): Boolean;
var
  Event: TEthereumContractEvent;
begin
  Event := GetEvent('type_bool');
  if Assigned(Event) then
    try
      Result := ParseCallCode(Event.EventName, Event.Events[Index].Data, Event.Parameters);
      if Result then
        begin
          value := Event.Parameters[0].AsBoolean;
        end;
    except
      on E: Exception do
        Result := ErrorParamConvert(Event.EventName, E);
    end else
    Result := ErrorNotFound('type_bool');
end;


function TEth_ContractDemoContract.FilterEvent_type_string(const FromBlockNumber: TEth_BlockNumber; const FromBlockNumberCustom: Int64; const ToBlockNumber: TEth_BlockNumber; const ToBlockNumberCustom: Int64): Boolean;
var
  Topics: TArray<String>;
  Event: TEthereumContractEvent;
begin
  Event := GetEvent('type_string');
  if Assigned(Event) then
    begin
      Result := GetEventHash(Event);
      if Result then
        begin
          SetLength(Topics, 1);
          Topics[0] := Event.EventHash;
          Event.Events.Clear;
          Result := eth_getLogs(FromBlockNumber, FromBlockNumberCustom, ToBlockNumber, ToBlockNumberCustom, ContractAddress, Topics, Event.Events);
        end;
    end else
    Result := ErrorNotFound('type_string');
end;

function TEth_ContractDemoContract.GetEvent_type_string(const Index: Int64; out value: String): Boolean;
var
  Event: TEthereumContractEvent;
begin
  Event := GetEvent('type_string');
  if Assigned(Event) then
    try
      Result := ParseCallCode(Event.EventName, Event.Events[Index].Data, Event.Parameters);
      if Result then
        begin
          value := Event.Parameters[0].AsString;
        end;
    except
      on E: Exception do
        Result := ErrorParamConvert(Event.EventName, E);
    end else
    Result := ErrorNotFound('type_string');
end;


function TEth_ContractDemoContract.FilterEvent_type_bytes32(const FromBlockNumber: TEth_BlockNumber; const FromBlockNumberCustom: Int64; const ToBlockNumber: TEth_BlockNumber; const ToBlockNumberCustom: Int64): Boolean;
var
  Topics: TArray<String>;
  Event: TEthereumContractEvent;
begin
  Event := GetEvent('type_bytes32');
  if Assigned(Event) then
    begin
      Result := GetEventHash(Event);
      if Result then
        begin
          SetLength(Topics, 1);
          Topics[0] := Event.EventHash;
          Event.Events.Clear;
          Result := eth_getLogs(FromBlockNumber, FromBlockNumberCustom, ToBlockNumber, ToBlockNumberCustom, ContractAddress, Topics, Event.Events);
        end;
    end else
    Result := ErrorNotFound('type_bytes32');
end;

function TEth_ContractDemoContract.GetEvent_type_bytes32(const Index: Int64; out value: TByteDynArray): Boolean;
var
  Event: TEthereumContractEvent;
begin
  Event := GetEvent('type_bytes32');
  if Assigned(Event) then
    try
      Result := ParseCallCode(Event.EventName, Event.Events[Index].Data, Event.Parameters);
      if Result then
        begin
          value := Event.Parameters[0].AsBytes;
        end;
    except
      on E: Exception do
        Result := ErrorParamConvert(Event.EventName, E);
    end else
    Result := ErrorNotFound('type_bytes32');
end;


end.