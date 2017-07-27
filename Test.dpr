program Test;

uses
  System.StartUpCopy,
  FMX.Forms,
  frmTest in 'frmTest.pas' {Form3},
  DEthereum.Types in 'DEthereum.Types.pas',
  DEthereum in 'DEthereum.pas' {/  ,  DemoContract;},
  DemoContract in 'DemoContract.pas';

//  ,  DemoContract;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
