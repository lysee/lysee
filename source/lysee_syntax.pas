{==============================================================================}
{        UNIT: lysee_synhighlighter                                            }
{ DESCRIPTION: lysee syntax highlighter for SynEdit                            }
{   COPYRIGHT: Copyright (c) 2008-2015, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2008/04/05                                                      }
{    MODIFIED: 2016/11/17                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lysee_syntax;

{$IFDEF FPC}
{$MODE objfpc}{$H+}
{$ENDIF}

interface

uses
  SysUtils, Classes, Controls, Graphics,
  {$IFDEF LYSEE_LAZ}LResources,{$ENDIF}
  SynEditTypes, SynEditHighlighter;

const
  SYNS_FILTERLANGUAGE = 'Lysee *.ls|*.ls';
  SYNS_LYSEELANGUAGE = 'Lysee';

type
  TTokenID = (tkUnknown, tkWrong, tkComment, tkID, tkKey, tkNull,
              tkSpace, tkParen, tkString, tkNumber);
  TRangeID = (rkUnknown, rkString, rkComment, rkParenComment);

  { TLiLyseeSyn }

  TLiLyseeSyn = class(TSynCustomHighlighter)
  private
    FLineString: string;
    FLine: pchar;
    FLineNumber: integer;
    FRun: longint;
    FTokenPos: integer;
    FTokenID: TTokenID;
    FRangeID: TRangeID;
    FAttrComment: TSynHighlighterAttributes;
    FAttrID: TSynHighlighterAttributes;
    FAttrWrong: TSynHighlighterAttributes;
    FAttrKeyword: TSynHighlighterAttributes;
    FAttrSpace: TSynHighlighterAttributes;
    FAttrString: TSynHighlighterAttributes;
    FAttrNumber: TSynHighlighterAttributes;
    FAttrParen: TSynHighlighterAttributes;
    FAttrUnknown: TSynHighlighterAttributes;
    procedure ParseCommentStart;
    procedure ParseComment;
    procedure ParseID;
    procedure ParseStringStart;
    procedure ParseString;
    procedure ParseNumber;
    procedure ParseParen;
    procedure ParseSpace;
    procedure ParseLineFeed;
    procedure ParseEnter;
    procedure ParseNull;
    procedure ParseUnknown;
  protected
    function GetIdentChars: TSynIdentChars;override;
    function IsFilterStored: Boolean;override;
    function NextChar: char;
  public
    class function GetLanguageName: string;override;
  public
    constructor Create(AOwner: TComponent);override;
    function GetRange: pointer;override;
    procedure ResetRange;override;
    procedure SetRange(Value: pointer);override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;override;
    function GetEol: boolean;override;
    function GetKeyWords: string;
    function GetTokenID: TTokenID;
    procedure SetLine({$IFDEF FPC}const{$ENDIF}NewValue: string; LineNumber: integer);override;
    function GetToken: string;override;
    procedure GetTokenEx(out TokenStart: pchar; out TokenLength: integer);override;
    function GetTokenAttribute: TSynHighlighterAttributes;override;
    function GetTokenKind: integer;override;
    function GetTokenPos: integer;override;
    procedure Next;override;
  published
    property AttrComment: TSynHighlighterAttributes read FAttrComment write FAttrComment;
    property AttrID: TSynHighlighterAttributes read FAttrID write FAttrID;
    property AttrKeyword: TSynHighlighterAttributes read FAttrKeyword write FAttrKeyword;
    property AttrSpace: TSynHighlighterAttributes read FAttrSpace write FAttrSpace;
    property AttrString: TSynHighlighterAttributes read FAttrString write FAttrString;
    property AttrNumber: TSynHighlighterAttributes read FAttrNumber write FAttrNumber;
    property AttrParen: TSynHighlighterAttributes read FAttrParen write FAttrParen;
    property AttrUnknown: TSynHighlighterAttributes read FAttrUnknown write FAttrUnknown;
  end;

procedure SetSynEditKeywords(const Keywords: string);

implementation

uses
  Clipbrd, SynEditStrConst;

var
  LyseeKeywords: TStringList;

procedure SetSynEditKeywords(const Keywords: string);
begin
  LyseeKeywords.Sorted := false;
  LyseeKeywords.CommaText := Keywords;
  LyseeKeywords.Sorted := true;
end;

{ TLiLyseeSyn}

procedure TLiLyseeSyn.ParseSpace;
begin
  FTokenID := tkSpace;
  while FLine[FRun] in [#1..#32] do Inc(FRun);
end;

procedure TLiLyseeSyn.ParseNull;
begin
  FTokenID := tkNull;
end;

procedure TLiLyseeSyn.ParseEnter;
begin
  FTokenID := tkSpace;
  Inc(FRun);
  if FLine[FRun] = #10 then
    Inc(FRun);
end;

procedure TLiLyseeSyn.ParseLineFeed;
begin
  FTokenID := tkSpace;
  Inc(FRun);
end;

procedure TLiLyseeSyn.ParseCommentStart;
begin
  if FLine[FRun] = '/' then
  begin
    FTokenID := tkComment;
    FRangeID := rkUnknown;
    repeat Inc(FRun) until FLine[FRun] in [#0, #10, #13];
  end
  else
  if FLine[FRun] = '{' then
  begin
    FTokenID := tkComment;
    FRangeID := rkComment;
    Inc(FRun);
  end
  else
  if FLine[FRun] = '(' then
  begin
    FTokenID := tkComment;
    FRangeID := rkParenComment;
    Inc(FRun);
  end
  else ParseUnknown;
end;

procedure TLiLyseeSyn.ParseComment;
begin
  FTokenID := tkComment;
  case FLine[FRun] of
     #0: ParseNull;
    #10: ParseLineFeed;
    #13: ParseEnter;
    else
    repeat
      if (FLine[FRun] = '}') and (FRangeID = rkComment) then
      begin
        Inc(FRun);
        FRangeID := rkUnknown;
        Exit;
      end
      else
      if (FLine[FRun] = '*') and (NextChar = ')') and (FRangeID = rkParenComment) then
      begin
        Inc(FRun, 2);
        FRangeID := rkUnknown;
        Exit;
      end
      else Inc(FRun);
    until FLine[FRun] in [#0, #10, #13];
  end;
end;

procedure TLiLyseeSyn.ParseStringStart;
begin
  FTokenID := tkString;
  FRangeID := rkString;
  Inc(FRun);
end;

procedure TLiLyseeSyn.ParseString;
begin
  FTokenID := tkString;
  case FLine[FRun] of
     #0: ParseNull;
    #10: ParseLineFeed;
    #13: ParseEnter;
    else
    repeat
      if FLine[FRun] = '''' then
      begin
        Inc(FRun);
        FRangeID := rkUnknown;
        Exit;
      end
      else Inc(FRun);
    until FLine[FRun] in [#0, #10, #13];
  end;
end;

constructor TLiLyseeSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAttrID := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(FAttrID);

  FAttrComment := TSynHighLighterAttributes.Create(SYNS_AttrComment);
  FAttrComment.Foreground := clOlive;
  AddAttribute(FAttrComment);

  FAttrWrong := TSynHighLighterAttributes.Create(SYNS_AttrUser);
  FAttrWrong.Foreground := clWhite;
  FAttrWrong.Background := clRed;
  AddAttribute(FAttrWrong);

  FAttrKeyword := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord);
  FAttrKeyword.Style := [fsBold];
  AddAttribute(FAttrKeyword);

  FAttrSpace := TSynHighLighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(FAttrSpace);

  FAttrString := TSynHighLighterAttributes.Create(SYNS_AttrString);
  FAttrString.Foreground := clBlue;
  AddAttribute(FAttrString);

  FAttrNumber := TSynHighLighterAttributes.Create(SYNS_AttrNumber);
  FAttrNumber.Foreground := clBlue;
  AddAttribute(FAttrNumber);

  FAttrParen := TSynHighLighterAttributes.Create(SYNS_AttrBrackets);
  FAttrParen.Foreground := clRed;
  AddAttribute(FAttrParen);

  FAttrUnknown := TSynHighLighterAttributes.Create(SYNS_AttrUnknownWord);
  FAttrUnknown.Foreground := clRed;
  AddAttribute(FAttrUnknown);

  SetAttributesOnChange({$IFDEF FPC}@{$ENDIF}DefHighlightChange);
  fDefaultFilter := SYNS_FILTERLANGUAGE;
  FRangeID := rkUnknown;
end;

procedure TLiLyseeSyn.SetLine({$IFDEF FPC}const {$ENDIF}NewValue: string; LineNumber: integer);
begin
  FLineString := NewValue;
  FLine := pchar(FLineString);
  FRun := 0;
  FLineNumber := LineNumber;
  if FLineNumber = 0 then
  begin
    FTokenID := tkUnknown;
    FRangeID := rkUnknown;
  end;
  Next;
end;

procedure TLiLyseeSyn.ParseID;
var
  base: integer;
  temp: string;
begin
  base := FRun;
  while FLine[FRun] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '@'] do Inc(FRun);
  SetString(temp, FLine + base, FRun - base);
  if LyseeKeywords.IndexOf(temp) >= 0 then
    FTokenID := tkKey else
    FTokenID := tkID;
end;

procedure TLiLyseeSyn.ParseUnknown;
begin
  FTokenID := tkUnknown;
  {$IFDEF SYN_MBCSSUPPORT}
  while FLine[FRun] in LeadBytes do Inc(FRun);
  {$ELSE}
  Inc(FRun);
  {$ENDIF}
end;

procedure TLiLyseeSyn.Next;
begin
  FTokenPos := FRun;
  if FRangeID = rkString then ParseString else
  if FRangeID in [rkComment, rkParenComment] then ParseComment else
  begin
    FRangeID := rkUnknown;
    case FLine[FRun] of
       #0: ParseNull;
      #10: ParseLineFeed;
      #13: ParseEnter;
      '/': if NextChar = '/' then ParseCommentStart else ParseUnknown;
      '{': ParseCommentStart;
     '''': ParseStringStart;
      '0'..'9': ParseNumber;
      '(': if NextChar = '*' then ParseCommentStart else ParseParen;
      ')', '[', ']': ParseParen;
      'A'..'Z', 'a'..'z', '_': ParseID;
      #1..#9, #11, #12, #14..' ': ParseSpace;
      else ParseUnknown;
    end;
  end;
end;

function TLiLyseeSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT    : Result := FAttrComment;
    SYN_ATTR_IDENTIFIER : Result := FAttrID;
    SYN_ATTR_KEYWORD    : Result := FAttrKeyword;
    SYN_ATTR_STRING     : Result := FAttrString;
    SYN_ATTR_WHITESPACE : Result := FAttrSpace;
    SYN_ATTR_NUMBER     : Result := FAttrNumber;
    SYN_ATTR_SYMBOL     : REsult := FAttrParen;
    else                  Result := FAttrSpace;
  end;
end;

function TLiLyseeSyn.GetEol: boolean;
begin
  Result := (FTokenID = tkNull)
end;

function TLiLyseeSyn.GetKeyWords: string;
begin
  Result := LyseeKeywords.CommaText;
end;

function TLiLyseeSyn.GetToken: string;
begin
  SetString(Result, (FLine + FTokenPos), FRun - FTokenPos);
end;

procedure TLiLyseeSyn.GetTokenEx(out TokenStart: pchar; out TokenLength: integer);
begin
  TokenLength := FRun - FTokenPos;
  if TokenLength > 0 then
    TokenStart := FLine + FTokenPos else
    TokenStart := nil;
end;

function TLiLyseeSyn.GetTokenID: TTokenID;
begin
  Result := FTokenID;
end;

function TLiLyseeSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  Result := FAttrUnknown;
  case GetTokenID of
    tkComment: Result := FAttrComment;
    tkID     : Result := FAttrID;
    tkWrong  : Result := FAttrWrong;
    tkKey    : Result := FAttrKeyword;
    tkSpace  : Result := FAttrSpace;
    tkString : Result := FAttrString;
    tkNumber : Result := FAttrNumber;
    tkParen  : Result := FAttrParen;
    tkUnknown: Result := FAttrUnknown;
  end;
end;

function TLiLyseeSyn.GetTokenKind: integer;
begin
  Result := Ord(FTokenID);
end;

function TLiLyseeSyn.GetTokenPos: integer;
begin
  Result := FTokenPos;
end;

function TLiLyseeSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', 'a'..'z', 'A'..'Z', '0'..'9', '@'];
end;

function TLiLyseeSyn.IsFilterStored: Boolean;
begin
  Result := (fDefaultFilter <> SYNS_FILTERLANGUAGE);
end;

function TLiLyseeSyn.NextChar: char;
begin
  if FRun < Length(FLine) - 1 then
    Result := FLine[FRun + 1] else
    Result := #0;
end;

{$IFNDEF SYN_CPPB_1} class {$ENDIF}
function TLiLyseeSyn.GetLanguageName: string;
begin
  Result := SYNS_LYSEELANGUAGE;
end;

procedure TLiLyseeSyn.ResetRange;
begin
  FRangeID := rkUnknown;
end;

procedure TLiLyseeSyn.SetRange(Value: pointer);
begin
  FRangeID := TRangeID(PtrUInt(Value));
end;

function TLiLyseeSyn.GetRange: pointer;
begin
  Result := pointer(PtrInt(FRangeID));
end;

procedure TLiLyseeSyn.ParseNumber;
var
  S: set of char;
  H: char;
begin
  FTokenID := tkNumber;
  H := FLine[FRun];
  Inc(FRun);
  if (H = '0') and (FLine[FRun] in ['x', 'X']) then
  begin
    S := ['0'..'9', 'a'..'f', 'A'..'F'];
    Inc(FRun);
  end
  else S := ['0'..'9'];
  while FLine[FRun] in S do Inc(FRun);
  FRangeID := rkUnknown;
end;

procedure TLiLyseeSyn.ParseParen;
begin
  FRangeID := rkUnknown;
  FTokenID := tkParen;
  Inc(FRun);
end;

initialization
  LyseeKeywords := TStringList.Create;
  LyseeKeywords.Sorted := true;
  SetSynEditKeywords(
    'and,as,begin,boolean,break,case,const,continue,dec,def,div,do,downto,' +
    'elif,else,end,except,exit,false,finally,float,for,get,if,in,inc,int,is,' +
    'like,main,mod,nil,not,object,of,or,raise,repeat,result,set,shl,shr,' +
    'string,system,then,to,true,try,type,until,uses,var,variant,while,xor');

finalization
  FreeAndNil(LyseeKeyWords);

end.
