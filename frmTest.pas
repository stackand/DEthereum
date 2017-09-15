unit frmTest;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, Generics.Collections,
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
  DEthereum, DEthereum.Types, DemoContract, System.Math.Vectors, FMX.Controls3D,
  FMX.Objects3D;

type
  TForm3 = class(TForm)
    TimerHashRate: TTimer;
    TabControl1: TTabControl;
    TabItemLog: TTabItem;
    PanelSetup: TPanel;
    EditServer: TEdit;
    NumberBoxPort: TNumberBox;
    FloatAnimation1: TFloatAnimation;
    ComboBoxFromAddress: TComboBox;
    EditFromPassword: TEdit;
    PasswordEditButton1: TPasswordEditButton;
    ButtonStartMiner: TButton;
    ButtonStopMiner: TButton;
    CheckBoxHashRate: TCheckBox;
    StringGridLog: TStringGrid;
    TimeColumn1: TTimeColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    TabItemTextLog: TTabItem;
    MemoTextLog: TMemo;
    TabItemABI: TTabItem;
    TabControlABI: TTabControl;
    TabItemABIFull: TTabItem;
    MemoABIFull: TMemo;
    TabItemABIShort: TTabItem;
    MemoABIShort: TMemo;
    TabItemABICustom: TTabItem;
    MemoABICustom: TMemo;
    TabItemABIResult: TTabItem;
    TabControl2: TTabControl;
    TabItemDelphiSource: TTabItem;
    MemoABIDelphiSource: TMemo;
    ButtonSaveDelphi: TButton;
    TabItemABIBuildResult: TTabItem;
    MemoContractABI: TMemo;
    EditContractAddress: TEdit;
    ButtonProcessABI: TButton;
    TabItemSolidity: TTabItem;
    StatusBar1: TStatusBar;
    ButtonCompileSolidity: TButton;
    TabControlSolidity: TTabControl;
    TabItemSoliditySource: TTabItem;
    MemoSoliditySource: TMemo;
    TabItemSolidityResult: TTabItem;
    MemoSolidityResult: TMemo;
    TabTransaction: TTabItem;
    PanelTransaction: TPanel;
    EditTransaction: TEdit;
    EditButtonTransaction: TEditButton;
    StringGridTransation: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    Block: TTabItem;
    Panel1: TPanel;
    EditBlock: TEdit;
    EditButtonBlock: TEditButton;
    StringGridBlock: TStringGrid;
    StringColumn5: TStringColumn;
    StringColumn6: TStringColumn;
    TabDemo: TTabItem;
    TabControl3: TTabControl;
    TabDemoEvents: TTabItem;
    StringGridEvents: TStringGrid;
    EventName: TStringColumn;
    EventValues: TStringColumn;
    TabDemoMethods: TTabItem;
    StringGridMethods: TStringGrid;
    StringColumn7: TStringColumn;
    StringColumn8: TStringColumn;
    StringColumn9: TStringColumn;
    StringColumn10: TStringColumn;
    StringColumn11: TStringColumn;
    procedure ButtonProcessABIClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EditServerChange(Sender: TObject);
    procedure ButtonCompileSolidityClick(Sender: TObject);
    procedure ComboBoxFromAddressClick(Sender: TObject);
    procedure ButtonStartMinerClick(Sender: TObject);
    procedure ButtonStopMinerClick(Sender: TObject);
    procedure EditButtonTransactionClick(Sender: TObject);
    procedure TimerHashRateTimer(Sender: TObject);
    procedure EditButtonBlockClick(Sender: TObject);
    procedure CallCodeButtonClick(Sender: TObject);
    procedure ButtonSaveDelphiClick(Sender: TObject);
    procedure TabDemoEventsClick(Sender: TObject);
    procedure StringGridEventsCellClick(const Column: TColumn;
      const Row: Integer);
    procedure TabDemoMethodsClick(Sender: TObject);
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

  MemoTextLog.Lines.Add(DateTimeToStr(Now));
  MemoTextLog.Lines.Add(&Type);
  MemoTextLog.Lines.Add(Text);

  MemoTextLog.Lines.Add('');
  MemoTextLog.Lines.Add('');
  MemoTextLog.Lines.Add('');
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

procedure TForm3.StringGridEventsCellClick(const Column: TColumn;
  const Row: Integer);
var
  i, j: Integer;
  b: Boolean;
  s: String;
  EventHash: String;
  bytes32: TByteDynArray;
  Topics: TArray<String>;
  Event: TEthereumContractEvent;
  Events: TObjectList<TEth_FilterChangeClass>;
begin
  Event := Demo.Event[StringGridEvents.Cells[0, Row]];
  Events := TObjectList<TEth_FilterChangeClass>.Create(nil);
  try
    if Assigned(Event) and Demo.GetEventHash(Event) then
      begin
        SetLength(Topics, 1);
        Topics[0] := Event.EventHash;
        if Demo.eth_getLogs(ethbnEearliest, 0, ethbnLatest, 0, Demo.ContractAddress, Topics, Events) then
          begin
            for i := 0 to Events.Count - 1 do
              s := s + Format('%s(%d)', [Events[i].blockNumber, j]);
            StringGridEvents.Cells[1, Row] := s;
          end;
      end;
  finally
    Events.Free;
  end;
end;

procedure TForm3.TabDemoMethodsClick(Sender: TObject);
var
  i: Integer;
begin
  if StringGridMethods.RowCount = 0 then
    for i := 0 to Demo.Methods.Count - 1 do
      begin
        StringGridMethods.RowCount := StringGridMethods.RowCount + 1;
        StringGridMethods.Cells[0, StringGridMethods.RowCount - 1] := Demo.Methods[i].EthereumMethodSignature;
        StringGridMethods.Cells[1, StringGridMethods.RowCount - 1] := Demo.Methods[i].MethodType;
        StringGridMethods.Cells[2, StringGridMethods.RowCount - 1] := BoolToStr(Demo.Methods[i].MethodPayable, True);
        StringGridMethods.Cells[3, StringGridMethods.RowCount - 1] := BoolToStr(Demo.Methods[i].MethodConstant, True);
        StringGridMethods.Cells[4, StringGridMethods.RowCount - 1] := '***';
      end;
end;

procedure TForm3.TabDemoEventsClick(Sender: TObject);
var
  i: Integer;
begin
  if StringGridEvents.RowCount = 0 then
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
