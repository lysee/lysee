{==============================================================================}
{     PROJECT: lysee_syncobj                                                   }
{ DESCRIPTION: syncronisizing objects and functions                            }
{   COPYRIGHT: Copyright (c) 2003-2012, Li Yun Jie. All Rights Reserved.       }
{     LICENSE: modified BSD license                                            }
{     CREATED: 2003/12/10                                                      }
{    MODIFIED: 2013/09/16                                                      }
{==============================================================================}
{ Contributor(s):                                                              }
{==============================================================================}
unit lmsyncobj;

{$mode objfpc}{$H+}

interface

uses
  lsyncobj, lysee;

{@pmc-description spin lock class}
{@pmc-object spinlock => spinlock}

type

  { spinlock }

  spinlock = class(TLseSpinLock)
  public
    {$IFDEF PMC_AVAILABLE}
    constructor Create;
    procedure Enter;
    procedure Leave;
    function TryEnter: boolean;
    {$ENDIF}
  end;

implementation

end.

