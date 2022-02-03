object dmServer: TdmServer
  OldCreateOrder = False
  Height = 296
  Width = 352
  object svrHTTP: TROIndyHTTPServer
    Dispatchers = <
      item
        Name = 'msgBIN'
        Message = msgBIN
        Enabled = True
        PathInfo = 'Bin'
      end>
    SendClientAccessPolicyXml = captAllowAll
    IndyServer.Bindings = <>
    IndyServer.DefaultPort = 8099
    Port = 8099
    Left = 64
    Top = 32
  end
  object msgBIN: TROBinMessage
    Envelopes = <>
    Left = 176
    Top = 32
  end
  object cmConnection: TDAConnectionManager
    Connections = <
      item
        Name = 'TestConn'
        ConnectionString = 
          'FireDAC?AuxDriver=PG;Server=192.168.0.98;Database=fpctest;UserID' +
          '=XXX;Password=YYY;'
        Description = 'Test database'
        ConnectionType = 'PostgreSQL'
        Default = True
      end>
    DriverManager = dmDriver
    PoolingEnabled = True
    Left = 64
    Top = 112
  end
  object dmDriver: TDADriverManager
    DriverDirectory = '%SYSTEM%\'
    AutoLoad = True
    TraceActive = False
    TraceFlags = []
    Left = 64
    Top = 176
  end
  object drvFireDAC: TDAFireDACDriver
    Left = 64
    Top = 232
  end
  object smMemory: TROEventSessionManager
    Left = 176
    Top = 128
  end
end
