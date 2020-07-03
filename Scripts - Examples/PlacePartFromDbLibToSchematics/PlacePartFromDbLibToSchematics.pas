{
  Original code from Allen GONG
  https://forum.live.altium.com/#posts/241580/746763
}

Procedure PlacePartFromDbLibToSchematics;
Begin
    ResetParameters;
    AddStringParameter('PartId','102OO-00001OE.01');
    AddStringParameter('LibReference', 'Socket-ATX-6');
    AddStringParameter('Library', 'YourDBLIB.DbLib');
    AddStringParameter('CurFootprint', 'Socket-ATX-6');
    AddStringParameter('SourceLibraryName', 'YourDBLIB.DbLib');
    AddStringParameter('DatabaseTableName', 'ATX');
    RunProcess('Sch:PlaceIntegratedComponentFromDB');
End;
