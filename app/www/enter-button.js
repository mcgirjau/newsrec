// Function to associate Return / Enter key with action buttons
$(document).on("keydown",
function (event) {
  if (event.keyCode == 13) {
    Shiny.onInputChange("login", new Date());
    Shiny.onInputChange("generate", new Date());
  }
});