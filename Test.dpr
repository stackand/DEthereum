program Test;

uses
  System.StartUpCopy,
  FMX.Forms,
  frmMain in 'frmMain.pas' {formMain},
  DEthereum.Types in 'DEthereum.Types.pas',
  frmBlock in 'frmBlock.pas' {formBlock: TFrame};

//  ,DemoContract;


{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TformMain, formMain);
  Application.Run;
end.
