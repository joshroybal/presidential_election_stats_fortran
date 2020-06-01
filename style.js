/*
function numberWithCommas(x) {
    return x.toString().replace(/\B(?=(\d{3})+(?!\d))/g, ",");
}
*/

function FormatTable(table)
{
   table.classList.add("gradienttable-mini");

   for (var i = 1, row; row = table.rows[i]; i++) {
      // iterate through rows
      // rows would be accessed using the "row" variable assigned in the for loop
      for (var j = 0, col; col = row.cells[j]; j++) {
         //iterate through columns
         //columns would be accessed using the "col" variable assigned in the for loop
         if ( isNaN(col.innerHTML) ) {
            col.outerHTML = "<td style='text-align:left;'>" + col.innerHTML + "</td>";
         } else {
            // col.outerHTML = "<td style='text-align:right;'>" + numberWithCommas(col.innerHTML) + "</td>";
            col.outerHTML = "<td style='text-align:right;'>" + col.innerHTML + "</td>";
         }
      }
   }
}

function AddStyle()
{
   var stylesheet = document.getElementById("styleinfo");
   stylesheet.innerHTML = "<link rel='stylesheet' media='all' type='text/css' href='style.css'/>";

   var table1 = document.getElementById("stats_table");
   var table2 = document.getElementById("swings_table");
	
	FormatTable(table1)
	FormatTable(table2)
}

window.onload = AddStyle;
