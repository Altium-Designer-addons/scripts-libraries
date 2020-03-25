Procedure StartScript ();
var
   Board              : IPCB_Board;         //Переменная объекта печатной платы
   X                  : Tcoord;             //Координата X - точка выбора компонента
   Y                  : Tcoord;             //Координата Y - точка выбора компонента

   ComponentIterator  : IPCB_BoardIterator; //Итератор компонентов
   Comp               : IPCB_Component;     //Компонент полученный через итератор
   CompItog           : IPCB_Component;     //Найденный компонент
   X1                 : Tcoord;             //Координата X - Текуший компонент в итераторе
   Y1                 : Tcoord;             //Координата Y - Текуший компонент в итераторе
   d                  : real;               //Длина до компонента
   dOld               : real;               //Длина до компонента с минимальной длиной

   lyrMehPairs        : IPCB_MechanicalLayerPairs; // Переменная содержащая пары механических слоев
   LayerM1            : Tlayer;                    // Первый механический слой
   LayerM2            : Tlayer;                    // Второй механический слой
   CurrentLayer       : integer;            //Индекс текущего слоя

   Area               : Tcoord;


Begin  // StartScript
       CurrentLayer  := eTopLayer;  //Инициализация переменной текущего слоя значением по умолчанию

       Board := PCBServer.GetCurrentPCBBoard;

       if Board = nil then // проверка на наличие открытой печатной платы
          Begin
               ShowError('Open Board!');
               Exit;
          end;

       Area := Board.SnapGridSize*2.1;
       Board.ChooseLocation(X,Y,'Choose Component'); //Выбираем компонент на плате

       //*******Определение рабочей стороны платы*****
       if (board.CurrentLayer = eTopLayer) | (board.CurrentLayer = eTopPaste) | (board.CurrentLayer =eTopOverlay)
       then CurrentLayer  := eTopLayer;
       if (board.CurrentLayer = eBottomLayer) | (board.CurrentLayer = eBottomPaste) | (board.CurrentLayer =eBottomOverlay)
       then CurrentLayer  := eBottomLayer;
       lyrMehPairs := Board.MechanicalPairs;
       for LayerM1 := 1 to 32 do
           for LayerM2 := LayerM1 to 32 do
           begin
                if lyrMehPairs.PairDefined(PCBServer.LayerUtils.MechanicalLayer(LayerM1),PCBServer.LayerUtils.MechanicalLayer(LayerM2)) then
                begin
                     if Board.CurrentLayer =  PCBServer.LayerUtils.MechanicalLayer(LayerM1) then CurrentLayer  := eTopLayer;
                     if Board.CurrentLayer =  PCBServer.LayerUtils.MechanicalLayer(LayerM2) then CurrentLayer  := eBottomLayer;
                end;
           end;
       //******Конец определения рабочей стороны платы*****

       ComponentIterator := Board.BoardIterator_Create; // Перебор всех обьектов на плате
       ComponentIterator.AddFilter_ObjectSet(MkSet(eComponentObject));  //фильтр перебора только компонентов.
       ComponentIterator.AddFilter_LayerSet(MkSet(CurrentLayer));        //Фильтр выбора компонентов на определенном слое.
       ComponentIterator.AddFilter_Method(eProcessAll);                 //Метод перебора.

       Comp := ComponentIterator.FirstPCBObject;                        //Получение первого компонента.
       CompItog := Comp;                                                //Инициализация первого значения.
       X1 := Comp.x;
       Y1 := Comp.y;
       dOld := sqrt(sqr(X1-X)+sqr(Y1-Y));
       While (Comp <> Nil) Do  //Цикл перебора компонентов
       Begin
            X1:=Comp.x;
            Y1:=Comp.y;
            d := sqrt(sqr(X1-X)+sqr(Y1-Y));
            if d <= Area then   // Оптимизация работы скрипта при точном указании компонента.
            begin
               CompItog := Comp;
               Break;
            end;

            if d < dOld then   // Поиск компонента по принципу - ближайший к точке
            begin
                 CompItog := Comp;
                 dOld := d;
            end;
        Comp := ComponentIterator.NextPCBObject;
       end;
       Board.BoardIterator_Destroy(ComponentIterator);

      If CompItog <> Nil then CompItog.Selected := true;
      Board.ViewManager_FullUpdate;
End;
