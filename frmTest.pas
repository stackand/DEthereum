unit frmTest;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  REST.Json,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.StdCtrls, FMX.TabControl, FMX.EditBox, FMX.NumberBox, FMX.Edit, FMX.Ani,
  FMX.ListBox, System.Rtti, FMX.Grid.Style, FMX.Grid, Data.Bind.Controls,
  FMX.Layouts, Fmx.Bind.Navigator, Data.Bind.Components, Data.Bind.DBScope,
  Data.DB, Data.Bind.EngExt, Fmx.Bind.DBEngExt, FireDAC.Stan.Intf,
  FireDAC.Stan.Option, FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS,
  FireDAC.Phys.Intf, FireDAC.DApt.Intf, FireDAC.Stan.StorageBin,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client,
  DEthereum, DEthereum.Types, DemoContract;

type
  TForm3 = class(TForm)
    TabControl1: TTabControl;
    TabItemABI: TTabItem;
    TabItemLog: TTabItem;
    TabItemSolidity: TTabItem;
    StatusBar1: TStatusBar;
    TabControlABI: TTabControl;
    TabItemABIFull: TTabItem;
    TabItemABIShort: TTabItem;
    MemoABIFull: TMemo;
    MemoABIShort: TMemo;
    TabItemABIResult: TTabItem;
    ButtonProcessABI: TButton;
    PanelSetup: TPanel;
    EditServer: TEdit;
    NumberBoxPort: TNumberBox;
    FloatAnimation1: TFloatAnimation;
    ButtonCompileSolidity: TButton;
    TabControlSolidity: TTabControl;
    TabItemSoliditySource: TTabItem;
    TabItemSolidityResult: TTabItem;
    MemoSoliditySource: TMemo;
    MemoSolidityResult: TMemo;
    TabItemTransaction: TTabItem;
    TabItemABICustom: TTabItem;
    MemoABICustom: TMemo;
    PanelTransaction: TPanel;
    EditTransaction: TEdit;
    ComboBoxFromAddress: TComboBox;
    EditFromPassword: TEdit;
    ButtonStartMiner: TButton;
    ButtonStopMiner: TButton;
    EditButtonTransaction: TEditButton;
    StringGridTransation: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    TabItemEvents: TTabItem;
    ButtonInfo: TButton;
    ssh_post: TButton;
    PasswordEditButton1: TPasswordEditButton;
    StringGridLog: TStringGrid;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    TimeColumn1: TTimeColumn;
    TimerHashRate: TTimer;
    Block: TTabItem;
    Panel1: TPanel;
    EditBlock: TEdit;
    EditButtonBlock: TEditButton;
    StringGridBlock: TStringGrid;
    StringColumn5: TStringColumn;
    StringColumn6: TStringColumn;
    set_int32: TButton;
    ButtonTotalEvents: TButton;
    CheckBoxHashRate: TCheckBox;
    CallCode: TTabItem;
    CallCodeEdit: TEdit;
    CallCodeButton: TEditButton;
    TabControl2: TTabControl;
    TabItemDelphiSource: TTabItem;
    TabItemABIBuildResult: TTabItem;
    MemoABIDelphiSource: TMemo;
    MemoContractABI: TMemo;
    ButtonSaveDelphi: TButton;
    StringGridEvents: TStringGrid;
    EventName: TStringColumn;
    EventValues: TStringColumn;
    TabItemContract: TTabItem;
    EditContractAddress: TEdit;
    procedure ButtonProcessABIClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonInfoClick(Sender: TObject);
    procedure ssh_postClick(Sender: TObject);
    procedure EditServerChange(Sender: TObject);
    procedure ButtonCompileSolidityClick(Sender: TObject);
    procedure ComboBoxFromAddressClick(Sender: TObject);
    procedure ButtonStartMinerClick(Sender: TObject);
    procedure ButtonStopMinerClick(Sender: TObject);
    procedure EditButtonTransactionClick(Sender: TObject);
    procedure TimerHashRateTimer(Sender: TObject);
    procedure EditButtonBlockClick(Sender: TObject);
    procedure set_int32Click(Sender: TObject);
    procedure ButtonTotalEventsClick(Sender: TObject);
    procedure CallCodeButtonClick(Sender: TObject);
    procedure ButtonSaveDelphiClick(Sender: TObject);
    procedure TabItemEventsClick(Sender: TObject);
    procedure StringGridEventsCellClick(const Column: TColumn;
      const Row: Integer);
  private
    { Private declarations }
    eth: TEthereum;
    demo: TEth_ContractDemoContract;
    function BlockInfo(Bl: TEth_BlockClass): TArray<String>;
    function TransactionInfo(Tr: TEth_TransactionClass): TArray<String>;
    procedure InfoToStringGrid(Info: TArray<String>; StringGrid: TStringGrid; ClearBefore: Boolean);
    procedure OnMethodError(Sender: TObject);
    procedure OnMethodCall(Sender: TObject);
    procedure OnMethodResult(Sender: TObject);
    procedure AddLog(&Type, Text: String);
    procedure SetupEthereum(Eth: TEthereum);
  public
    { Public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.fmx}

procedure TForm3.AddLog(&Type, Text: String);
begin
  StringGridLog.RowCount := StringGridLog.RowCount + 1;
  StringGridLog.Cells[0, StringGridLog.RowCount - 1] := DateTimeToStr(Now);
  StringGridLog.Cells[1, StringGridLog.RowCount - 1] := &Type;
  StringGridLog.Cells[2, StringGridLog.RowCount - 1] := Text;
end;

procedure TForm3.set_int32Click(Sender: TObject);
var
  __set_int32: Integer;
begin
  if Demo.set_int32(1000000, 50000000, 112233445566, __set_int32) then
    ShowMessage(IntToStr(__set_int32));
end;

function TForm3.BlockInfo(Bl: TEth_BlockClass): TArray<String>;
begin
  Result := [
    'number',           Bl.number,
    'hash',             Bl.hash,
    'parentHash',       Bl.parentHash,
    'number',           Bl.number,
    'difficulty',       Bl.difficulty,
    'totalDifficulty',  Bl.totalDifficulty,
    'nonce',            Bl.nonce,
    'mixHash',          Bl.mixHash,
    'parentHash',       Bl.parentHash,
    'receiptsRoot',     Bl.receiptsRoot,
    'stateRoot',        Bl.stateRoot,
    'timestamp',        Bl.timestamp,
    'sha3Uncles',       Bl.sha3Uncles,
    'logsBloom',        Bl.logsBloom,
    'transactionsRoot', Bl.transactionsRoot
  ];
end;

procedure TForm3.ButtonCompileSolidityClick(Sender: TObject);
var
  s: String;
begin
  MemoSolidityResult.Lines.Clear;
  if eth.eth_compileSolidity(MemoSoliditySource.Lines.Text, s) then
    begin
      MemoABICustom.Lines.Text := s;
      MemoSolidityResult.Lines.Text := s;

      TabControlSolidity.ActiveTab := TabItemSolidityResult;
      TabControlABI.ActiveTab := TabItemABICustom;
    end;
end;

procedure TForm3.ButtonProcessABIClick(Sender: TObject);
var
  s: String;
  pa: Boolean;
  c: TEthereumContract;
begin

{  case TabControlABI.ActiveTab.Index of
    0: eth.ABIToContract('DemoContract', MemoABIFull.Text, MemoABIResult.Lines, 'DemoContract.pas');
    1: eth.ABIToContract('DemoContract', MemoABIShort.Text, MemoABIResult.Lines, 'DemoContract.pas');
    2: eth.ABIToContract('DemoContract', MemoABICustom.Text, MemoABIResult.Lines, 'DemoContract.pas');
  end;}

  c := TEthereumContract.Create;
  try
    case TabControlABI.ActiveTab.Index of
      0: pa := c.ParseABI(MemoABIFull.Text);
      1: pa := c.ParseABI(MemoABIShort.Text);
      2: pa := c.ParseABI(MemoABICustom.Text);
    end;
    if pa then
      begin
        c.BuildDelphiSource('DemoContract', MemoABIDelphiSource.Lines);
        if c.BuildABI(s, True) then
          begin
            MemoContractABI.Lines.Text := s;
            EditContractAddress.Text := c.ContractAddress;
          end;
      end;
  finally
    c.Free;
  end;

  TabControlABI.ActiveTab := TabItemABIResult;
end;

procedure TForm3.ButtonInfoClick(Sender: TObject);
var
  i: Integer;
  HashRate: Int64;
  s,v: String;
  o: TEth_syncingClass;
  Accounts: TArray<String>;
  eb: TEth_BlockClass;
  tc: TEth_TransactionClass;
  bc: TEth_BlockClass;
  trc: TEth_TransactionReceiptClass;
begin
  eth.web3_clientVersion(s);
//  eth.RpcCall('eth_blockNumber', []);
//  eth.RpcCall('eth_blockNumber', []);
  eth.eth_hashrate(HashRate);
  eth.eth_syncing(o);

  eth.eth_accounts(Accounts);
  eth.eth_getStorageAt('Accounts', 40, ethbnLatest, 0, s);

  eth.eth_getBlockByNumber(30000, True, eb);

//  eth.eth_compileSolidity('contract test { function multiply(uint a) returns(uint d) {   return a * 7;   } }', s);

  eth.eth_getTransactionByHash('0xfef7c572777ef61a4d419b23f3bc54523fec412129bc32f632b99481b99048dd', tc);
  eth.eth_getBlockByNumber(61483, True, bc);
  eth.eth_getTransactionReceipt('0xfef7c572777ef61a4d419b23f3bc54523fec412129bc32f632b99481b99048dd', trc);
  s := 'addPhoto(string,string,string,uint256,string,string)';
  v := '0x';
  for i := 1 to Length(s) do
    v := v + IntToHex(Ord(s[i]), 2);
  eth.web3_sha3([v], s);

end;

procedure TForm3.ButtonSaveDelphiClick(Sender: TObject);
begin
  MemoABIDelphiSource.Lines.SaveToFile('DemoContract.pas');
end;

procedure TForm3.ButtonStartMinerClick(Sender: TObject);
var
  Result: Boolean;
begin
  eth.miner_start(1, Result);
end;

procedure TForm3.ButtonStopMinerClick(Sender: TObject);
var
  Result: Boolean;
begin
  eth.miner_stop(Result);
end;

procedure TForm3.ButtonTotalEventsClick(Sender: TObject);
{var
  __totalEvents: Int64;
  i: Int64;
  Demo: TEth_ContractDemoContract;
  t: TArray<string>;}
begin
{  Demo := TEth_ContractDemoContract.Create;
  SetupEthereum(Demo);
//  t := TArray<String>.Create(''); Demo.Events[0].MethodHash
  try
//    Demo.eth_newFilter(ethbnEearliest, 0, ethbnLatest, 0, Demo.ContractAddress, t, i);
    if Demo.totalEvents(1000000, 50000000, __totalEvents) then
      begin
        ButtonTotalEvents.Text := __totalEvents.ToString;
      end else
      begin
        ButtonTotalEvents.Text := '????';
      end;
  finally
    Demo.Free;
  end;}
end;

procedure TForm3.CallCodeButtonClick(Sender: TObject);
var
  c: TEthereumContract;
begin
  c := TEthereumContract.Create;
  try
    //
  finally
    c.Free;
  end;
end;

procedure TForm3.ComboBoxFromAddressClick(Sender: TObject);
var
  Account: String;
  Accounts: TArray<String>;
begin
  if (ComboBoxFromAddress.Items.Count = 0) and eth.eth_accounts(Accounts) then
    begin
      ComboBoxFromAddress.Items.Clear;
      for Account in Accounts do
        ComboBoxFromAddress.Items.Add(Account);
    end;
end;

procedure TForm3.EditButtonBlockClick(Sender: TObject);
var
  i: Integer;
  Bl: TEth_BlockClass;
begin
  try
    if eth.eth_getBlockByHash(EditBlock.Text, True, Bl) and Assigned(Bl) then
      begin
        InfoToStringGrid(BlockInfo(Bl), StringGridBlock, True);

        for i := 0 to Length(Bl.transactions) - 1 do
          begin
            InfoToStringGrid([Format('Transaction %d', [i]), ''], StringGridBlock, False);
            InfoToStringGrid(TransactionInfo(Bl.transactions[i]), StringGridBlock, False);
          end;
      end;
  finally
    FreeAndNil(Bl);
  end;
end;

procedure TForm3.EditButtonTransactionClick(Sender: TObject);
var
  Tr: TEth_TransactionClass;
begin
  try
    if eth.eth_getTransactionByHash(EditTransaction.Text, Tr) and Assigned(Tr) then
      InfoToStringGrid(TransactionInfo(Tr), StringGridTransation, True);
  finally
    FreeAndNil(Tr);
  end;
end;

procedure TForm3.EditServerChange(Sender: TObject);
begin
  SetupEthereum(eth);
  SetupEthereum(demo);
end;

procedure TForm3.FormCreate(Sender: TObject);
begin
  eth := TEthereum.Create;
  demo := TEth_ContractDemoContract.Create;

  EditServerChange(nil);
end;

procedure TForm3.FormDestroy(Sender: TObject);
begin
  eth.Free;
  demo.Free;
end;

procedure TForm3.InfoToStringGrid(Info: TArray<String>; StringGrid: TStringGrid;
  ClearBefore: Boolean);
var
  i: Integer;
begin
  if ClearBefore then
    StringGrid.RowCount := 0;

  for i := 0 to (Length(Info) - 1) div 2 do
    begin
      StringGrid.RowCount := StringGrid.RowCount + 1;
      StringGrid.Cells[0, StringGrid.RowCount - 1] := Info[i * 2];
      StringGrid.Cells[1, StringGrid.RowCount - 1] := Info[i * 2 + 1];
    end;
end;

procedure TForm3.OnMethodCall(Sender: TObject);
begin
  AddLog('Call', (Sender as TEthereum).MethodCall.ToJSON);
end;

procedure TForm3.OnMethodError(Sender: TObject);
begin
  AddLog('Error', (Sender as TEthereum).MethodError.code.ToString + ': ' + (Sender as TEthereum).MethodError.message);
end;

procedure TForm3.OnMethodResult(Sender: TObject);
begin
  AddLog('Result', (Sender as TEthereum).MethodResult.ToJSON);
end;

procedure TForm3.SetupEthereum(Eth: TEthereum);
begin
  Eth.RpcAddress := EditServer.Text;
  Eth.RpcPort := Trunc(NumberBoxPort.Value);

  if ComboBoxFromAddress.ItemIndex <> - 1 then
    Eth.CoinAddress := ComboBoxFromAddress.Items[ComboBoxFromAddress.ItemIndex];
  Eth.CoinPassword := EditFromPassword.Text;

  Eth.OnMethodCall := OnMethodCall;
  Eth.OnMethodResult := OnMethodResult;
  Eth.OnMethodError := OnMethodError;
end;

procedure TForm3.ssh_postClick(Sender: TObject);
var
  sl: TStringList;
  PostResult: Boolean;
begin
  sl := TStringList.Create;

  sl.Add('1');
  sl.Add('2');
  sl.Add('3');

  eth.shh_post('from', 'to', sl, 'Message', 64, 64, PostResult);
end;

procedure TForm3.StringGridEventsCellClick(const Column: TColumn;
  const Row: Integer);
var
  b: Boolean;
  Event: String;
  bytes32: TByteDynArray;
begin
  Event := StringGridEvents.Cells[0, Row];
  if Event = 'type_int32' then b := Demo.FilterEvent_type_int32(ethbnEearliest, 0, ethbnLatest, 0) else
  if Event = 'type_int64' then b := Demo.FilterEvent_type_int64(ethbnEearliest, 0, ethbnLatest, 0) else
  if Event = 'type_bool' then b := Demo.FilterEvent_type_bool(ethbnEearliest, 0, ethbnLatest, 0) else
  if Event = 'type_string' then b := Demo.FilterEvent_type_string(ethbnEearliest, 0, ethbnLatest, 0) else
  if Event = 'type_bytes32' then b := Demo.FilterEvent_type_bytes32(ethbnEearliest, 0, ethbnLatest, 0) else
    ShowMessage('Unknown event name');

  if b then
    begin
      Demo.GetEvent_type_bytes32(0, bytes32);
      StringGridEvents.Cells[1, Row] := eth_bytesToHex(bytes32);
    end;
end;

procedure TForm3.TabItemEventsClick(Sender: TObject);
var
  i: Integer;
begin
  if StringGridEvents.RowCount = 0 then
    if Demo.FilterEvent_type_int32(ethbnEearliest, 0, ethbnLatest, 0) then
      for i := 0 to Demo.Events.Count - 1 do
        begin
          StringGridEvents.RowCount := StringGridEvents.RowCount + 1;
          StringGridEvents.Cells[0, StringGridEvents.RowCount - 1] := demo.Events[i].EventName;
        end;
end;

procedure TForm3.TimerHashRateTimer(Sender: TObject);
var
  Version: String;
  HashRate: Int64;
begin
  if CheckBoxHashRate.IsChecked
  and eth.web3_clientVersion(Version) and eth.eth_hashrate(HashRate) then
      Caption := Format('%s - %.2f Kh/s', [Version, HashRate/1000]);
end;

function TForm3.TransactionInfo(Tr: TEth_TransactionClass): TArray<String>;
begin
  Result := [
    'blockHash',          Tr.blockHash,
    'blockNumber',        Tr.blockNumber,
    'from',               Tr.from,
    'gas',                Tr.gas,
    'gasPrice',           Tr.gasPrice,
    'hash',               Tr.hash,
    'input',              Tr.input,
    'nonce',              Tr.nonce,
    'r',                  Tr.r,
    's',                  Tr.s,
    'transactionIndex',   Tr.transactionIndex,
    'v',                  Tr.v,
    'value',              Tr.value
  ];
end;

end.



{

procedure TForm3.ButtonContractClick(Sender: TObject);
var
  s: String;
  b: Boolean;
  eth: TEthContractDemo;
  txs: TTxpool_statusClass;
  ani: TAdmin_nodeInfoClass;
  ape: TAdmin_nodePeersClass;
begin
  eth := TEthContractDemo.Create;
  eth.OnMethodCall := OnMethodCall;
  eth.OnMethodResult := OnMethodResult;
  eth.OnMethodError := OnMethodError;

  eth.RpcAddress := EditServer.Text;
  eth.RpcPort := Trunc(NumberBoxPort.Value);

  eth.admin_peers(ape);
  exit;

  eth.admin_nodeInfo(ani);
  exit;

  eth.txpool_inspect(s);
  exit;
  eth.personal_newAccount('hello', s);
  eth.addPhoto('firstName', 'lastName', 'patronymicName', '1020', 'fullPhoto', 'thumbnailPhoto'+DateTimeToStr(Now));
  eth.Free;
end;

}
