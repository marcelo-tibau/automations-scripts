//Trigger to activate the menu
function createSpreadsheetOpenTrigger() {
  var ss = SpreadsheetApp.getActive();
  ScriptApp.newTrigger('createSpreadsheetMenu')
      .forSpreadsheet(ss)
      .onOpen()
      .create();
}

//Function to create the menu to be displayed at Spreadsheet
function createSpreadsheetMenu () {
  var ui = SpreadsheetApp.getUi();
  ui.createMenu('Cenarios')
      .addItem('Gerar', 'autoIncrement')
      .addToUi();
}


function autoIncrement () {
  var ss = SpreadsheetApp.openById('1fyWGPamiNe6gnx9U3ompeV9EaWx6LR5M2bszOP7IPnU');
  var source = ss.getRange ("Sheet1!A5:J9");
  var destSheet = ss.getSheetByName("Sheet1");
  
  var destRange = destSheet.getRange(destSheet.getLastRow()+2,1);
  source.copyTo (destRange, {contentsOnly: false});
  
  var r1 = ss.getRange('Sheet1!C2').getValue();
  var r2 = ss.getRange('Sheet1!E2').getValue();
  var r3 = ss.getRange('Sheet1!G2').getValue();
  
  var unCount = ss.getRange('Sheet1!C5');
  var count1 = ss.getRange('Sheet1!C6');
  var count2 = ss.getRange('Sheet1!C7');
  var count3 = ss.getRange('Sheet1!C8');
  
  var noAdd = unCount.getValue();
  unCount.setValue(noAdd-r1-r2-r3);
  var yesAdd1 = count1.getValue();
  count1.setValue(yesAdd1+r1);
  var yesAdd2 = count2.getValue();
  count2.setValue(yesAdd2+r2);
  var yesAdd3 = count3.getValue();
  count3.setValue(yesAdd3+r3);
  
}

