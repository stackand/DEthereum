unit frmBlock;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, FMX.Grid, FMX.ScrollBox, FMX.Edit,
  FMX.Controls.Presentation, FMX.TabControl,
  DEthereum, DEthereum.Types;

type
  TformBlock = class(TFrame)
    EditSearch: TEdit;
    SearchButton: TSearchEditButton;
    TabControl1: TTabControl;
    General: TTabItem;
    Transactions: TTabItem;
    StringGridBlock: TStringGrid;
    StringColumn5: TStringColumn;
    StringColumn6: TStringColumn;
    StringGridList: TStringGrid;
    StringColumn1: TStringColumn;
    StringColumn2: TStringColumn;
    StringColumn3: TStringColumn;
    StringColumn4: TStringColumn;
    StringColumn7: TStringColumn;
    procedure SearchButtonClick(Sender: TObject);
    procedure StringGridListCellDblClick(const Column: TColumn;
      const Row: Integer);
  private
    { Private declarations }
    function BlockInfo(Bl: TEth_BlockClass): TArray<String>;
  public
    { Public declarations }
  end;

implementation

uses
  frmMain,
  frmTransaction;

{$R *.fmx}

function TformBlock.BlockInfo(Bl: TEth_BlockClass): TArray<String>;
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


procedure TformBlock.SearchButtonClick(Sender: TObject);
var
  i: Integer;
  Bl: TEth_BlockClass;
  Tr: TEth_TransactionClass;
begin
  //SetTitle(EditTransaction.Text);
  Bl := nil;
  try
    if eth.eth_getBlockByHash(EditSearch.Text, True, Bl) and Assigned(Bl) then
      begin
        InfoToStringGrid(BlockInfo(Bl), StringGridBlock, True);
        for i := 0 to Length(Bl.transactions) - 1 do
          begin
            tr := Bl.transactions[i];
            RecordToStringGrid([tr.hash, tr.from, tr.&to, tr.value, tr.gas], StringGridList, i = 0);
          end;
      end;
  finally
    FreeAndNil(Bl);
  end;
end;

procedure TformBlock.StringGridListCellDblClick(const Column: TColumn;
  const Row: Integer);
begin
  if SameText(Column.Header, 'BlockHash') then
    NewTabBlock(StringGridList.Cells[Column.Index, StringGridList.Row]) else
  if SameText(Column.Header, 'TransactionHash') then
    NewTabTransaction(StringGridList.Cells[Column.Index, StringGridList.Row]);
end;

end.
