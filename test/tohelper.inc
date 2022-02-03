
  V:=value;
  AssertEquals('ToString',ValueAsString,V.ToString);
  AssertEquals('ToBoolean',True,V.ToBoolean);
  V:=0;
  AssertEquals('ToBoolean (false)',False,V.ToBoolean);
  V:=value;
  AssertEquals('ToHexString',ValueAshex,V.ToHexString);
  AssertEquals('ToHexString(digits)',ValueAsHexDigString,V.ToHexString(ValueAsHexDig));
  AssertEquals('ToDouble',Double(Value+0.0),V.ToDouble,0.00001);
  AssertEquals('ToExtended',Extended(Value+0.0),V.ToExtended,0.00001);

