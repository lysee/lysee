unit lmsockets;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sockets, lysee;

{@pmc-description socket utilities}
{@pmc-consts
  EsockEINTR  EsockEBADF  EsockEFAULT  EsockEINVAL  EsockEACCESS
  EsockEMFILE  EsockEMSGSIZE  EsockENOBUFS  EsockENOTCONN
  EsockENOTSOCK  EsockEPROTONOSUPPORT  EsockEWOULDBLOCK
  SHUT_RD  SHUT_WR  SHUT_RDWR
  SOCK_STREAM  SOCK_DGRAM  SOCK_RAW  SOCK_RDM  SOCK_SEQPACKET
  INADDR_ANY  INADDR_NONE
  S_IN  S_OUT
  INVALID_SOCKET
}

type

  { TLseSocket }

  TLseSocket = class(TLyObject)
  private
    FSocket: TSocket;
  public
    constructor Create(Domain, SocketType, Protocol:longint);
    destructor Destroy;override;
    {
    function recv(buf: pointer; len: longint; flags: longint):longint;
    function recvfrom(buf: pointer; len: longint; flags: longint; from : psockaddr; fromlen : psocklen):longint;
    function send(msg:pointer; len:longint; flags:longint):longint;
    function sendto(msg:pointer; len:longint; flagtox :psockaddr; tolen: tsocklen):longint;
    function bind(addrx : psockaddr; addrlen : tsocklen):longint;
    function listen(backlog : longint):longint;
    function accept(addrx : psockaddr; addrlen : psocklen):longint;
    function connect(name  : psockaddr; namelen : tsocklen):longint;
    function shutdown(how:longint):longint;
    function getsockname(name  : psockaddr; namelen : psocklen):longint;
    function getpeername(name  : psockaddr; namelen : psocklen):longint;
    function getsockopt(level:longint; optname:longint; optval:pointer; optlen : psocklen):longint;
    function setsockopt(level:longint; optname:longint; optval:pointer; optlen : tsocklen):longint;
    function socketpair(d:longint; xtype:longint; protocol:longint; sv:pcint):longint;
    }
  end;

implementation

{ TLseSocket }

constructor TLseSocket.Create(Domain, SocketType, Protocol: longint);
begin
  FSocket := fpsocket(Domain, SocketType, Protocol);
end;

destructor TLseSocket.Destroy;
begin
  if FSocket <> INVALID_SOCKET then
    CloseSocket(FSocket);
  inherited Destroy;
end;

end.

