unit prints;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

{$IFDEF MSWINDOWS}
{$IFNDEF WINDOWS}{$DEFINE WINDOWS}{$ENDIF}
{$ENDIF}

interface

uses
  {$IFDEF WINDOWS}Windows,{$ENDIF}
  SysUtils, Classes, Graphics, Printers;

type
  TPosUnit = (puMm, puInch, puDot);

function PosUnit: TPosUnit;
function SetPosUnit(NewUnit: TPosUnit): TPosUnit;

function HasPrinter: boolean;
function PrinterCount: integer;
procedure SetPaperSize(Width, Length: integer);overload; {<--unit: mm}
procedure SetPaperSize(Size: integer);overload;
procedure SetPortrait(Value: boolean = true);
function IsPortrait: boolean;
procedure SetLandscape(Value: boolean = true);
function IsLandscape: boolean;
function IsPrinting: boolean;
function isAborted: boolean;

function AveCharWidth: integer;
function HorzOffset: integer;
function VertOffset: integer;
function HorzDPI: integer;
function VertDPI: integer;
function HorzScale: currency;
function VertScale: currency;

function PosToX(X: currency): integer;
function PosToY(Y: currency): integer;
function PosToX_inch(X: currency): integer;
function PosToY_inch(Y: currency): integer;
function PosToX_mm(X: currency): integer;
function PosToY_mm(Y: currency): integer;
function MmToInch(Value: currency): currency;

procedure BeginPrint;
procedure EndPrint;
procedure AbortPrint;

procedure TextOut(X, Y: currency; const S: string);overload;
procedure TextOut(X, Y: integer; const S: string);overload;
procedure MoveTo(X, Y: currency);overload;
procedure MoveTo(X, Y: integer);overload;
procedure LineTo(X, Y: currency);overload;
procedure LineTo(X, Y: integer);overload;

implementation

uses
  Forms;

var
  chk_printer: boolean = false;
  has_printer: boolean = false;
  pos_unit: TPosUnit = puMm;

function PosUnit: TPosUnit;
begin
  Result := pos_unit;
end;

function SetPosUnit(NewUnit: TPosUnit): TPosUnit;
begin
  Result := pos_unit;
  pos_unit := NewUnit;
end;

function HasPrinter: boolean;
begin
  if not chk_printer then
  try
    chk_printer := true;
    has_printer := (Printer.Printers.Count > 0) and (Printer.PrinterIndex >= 0);
  except
    has_printer := false;
  end;
  Result := has_printer;
end;

function PrinterCount: integer;
begin
  if HasPrinter then
    Result := Printer.Printers.Count else
    Result := 0;
end;

procedure SetPaperSize(Width, Length: integer);
{$IFNDEF FPC}
var
  D, R, P: array[0..255] of char;
  H: THandle;
  M: PDeviceMode;
{$ENDIF}
begin
  if HasPrinter and (Width > 0) and (Length > 0) then
  begin
    Printer.PrinterIndex;
    {$IFDEF FPC}
    Exit;
    {$ELSE}
    Printer.GetPrinter(D, R, P, H);
    M := GlobalLock(H);
    if M <> nil then
    begin
      M^.dmFields := M^.dmFields or DM_PAPERSIZE
                                 or DM_PAPERLENGTH
                                 or DM_PAPERWIDTH
                                 or DMBIN_MANUAL;
      M^.dmPaperSize := DMPAPER_USER;
      M^.dmPaperWidth := Width * 10;
      M^.dmPaperLength := Length * 10;
      M^.dmDefaultSource := DMBIN_MANUAL;
      GlobalUnlock(H);
      Exit;
    end;
    {$ENDIF}
  end;
  raise Exception.Create('Failed setting paper size');
end;

procedure SetPaperSize(Size: integer);
{$IFNDEF FPC}
var
  D, R, P: array[0..255] of char;
  H: THandle;
  M: PDeviceMode;
{$ENDIF}
begin
  if HasPrinter and (Size > 0) then
  begin
    Printer.PrinterIndex;
    {$IFDEF FPC}
    Exit;
    {$ELSE}
    Printer.GetPrinter(D, R, P, H);
    M := GlobalLock(H);
    if M <> nil then
    begin
      M^.dmFields := M^.dmFields or DM_PAPERSIZE;
      M^.dmPaperSize := Size;
      GlobalUnlock(H);
      Exit;
    end;
    {$ENDIF}
  end;
  raise Exception.Create('Failed setting paper size');
end;

procedure SetPortrait(Value: boolean);
begin
  if HasPrinter then if Value then
    Printer.Orientation := poPortrait else
    Printer.Orientation := poLandscape;
end;

function IsPortrait: boolean;
begin
  Result := HasPrinter and (Printer.Orientation = poPortrait);
end;

procedure SetLandscape(Value: boolean);
begin
  if HasPrinter then if Value then
    Printer.Orientation := poLandscape else
    Printer.Orientation := poPortrait;
end;

function IsLandscape: boolean;
begin
  Result := HasPrinter and (Printer.Orientation = poLandscape);
end;

function IsPrinting: boolean;
begin
  Result := HasPrinter and Printer.Printing;
end;

function isAborted: boolean;
begin
  Result := HasPrinter and Printer.Aborted;
end;

function AveCharWidth: integer;
var
  M: TTextMetric;
begin
  GetTextMetrics(Printer.Canvas.Handle, M);
  Result := M.tmAveCharWidth;
end;

function HorzOffset: integer;
begin
  {$IFDEF FPC}
  Result := 0;
  {$ELSE}
  Result := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETX);
  {$ENDIF}
end;

function VertOffset: integer;
begin
  {$IFDEF FPC}
  Result := 0;
  {$ELSE}
  Result := GetDeviceCaps(Printer.Handle, PHYSICALOFFSETY);
  {$ENDIF}
end;

function HorzDPI: integer;
begin
  {$IFDEF FPC}
  Result := Printer.XDPI;
  {$ELSE}
  Result := GetDeviceCaps(Printer.Handle, LOGPIXELSX);
  {$ENDIF}
end;

function VertDPI: integer;
begin
  {$IFDEF FPC}
  Result := Printer.YDPI;
  {$ELSE}
  Result := GetDeviceCaps(Printer.Handle, LOGPIXELSY);
  {$ENDIF}
end;

function HorzScale: currency;
begin
  Result := HorzDPI / Screen.PixelsPerInch;
end;

function VertScale: currency;
begin
  Result := VertDPI / Screen.PixelsPerInch;
end;

function PosToX(X: currency): integer;
begin
  case pos_unit of
    puMm: Result := PosToX_mm(X);
    puInch: Result := PosToX_inch(X);
    else Result := Round(X);
  end;
end;

function PosToY(Y: currency): integer;
begin
  case pos_unit of
    puMm: Result := PosToY_mm(Y);
    puInch: Result := PosToY_inch(Y);
    else Result := Round(Y);
  end;
end;

function PosToX_inch(X: currency): integer;
begin
  Result := Round(X * HorzDPI - HorzOffset {+ 2 * AveCharWidth});
end;

function PosToY_inch(Y: currency): integer;
begin
  Result := Round(Y * VertDPI - VertOffset);
end;

function PosToX_mm(X: currency): integer;
begin
  Result := PosToX_inch(MmToInch(X));
end;

function PosToY_mm(Y: currency): integer;
begin
  Result := PosToY_inch(MmToInch(Y));
end;

function MmToInch(Value: currency): currency;
begin
  Result := Value / 25.4;
end;

procedure BeginPrint;
begin
  if HasPrinter then Printer.BeginDoc;
end;

procedure EndPrint;
begin
  if HasPrinter then Printer.EndDoc;
end;

procedure AbortPrint;
begin
  if HasPrinter then Printer.Abort;
end;

procedure TextOut(X, Y: currency; const S: string);
begin
  Printer.Canvas.TextOut(PosToX(X), PosToY(Y), S);
end;

procedure TextOut(X, Y: integer; const S: string);
begin
  Printer.Canvas.TextOut(X, Y, S);
end;

procedure MoveTo(X, Y: currency);
begin
  Printer.Canvas.MoveTo(PosToX(X), PosToY(Y));
end;

procedure MoveTo(X, Y: integer);
begin
  Printer.Canvas.MoveTo(X, Y);
end;

procedure LineTo(X, Y: currency);
begin
  Printer.Canvas.LineTo(PosToX(X), PosToY(Y));
end;

procedure LineTo(X, Y: integer);
begin
  Printer.Canvas.LineTo(X, Y);
end;

end.
