HTMLWidgets.widget({

  name: 'timelinevis',

  type: 'output',

  factory: function(el, width, height) {

    // define shared variables for this instance
    var timeline = new vis.Timeline(el);
    var items;
    var groups;
    var options = {};
    var id;

    return {

      renderValue: function(x) {

        if (x.id)
          id = x.id;

        // Create a DataSet (allows two way data-binding)
        items = new vis.DataSet(x.items);

        // Configuration for the Timeline
        options = x.options;
        options.onMove = function(item,callback) {
          Shiny.onInputChange("tlMoveEvent", {id:id, item:item});
          callback(item);
        };

        timeline.setItems(items);
        timeline.setOptions(options);

        if (x.groups) {
           groups = new vis.DataSet(x.groups);
           timeline.setGroups(groups);
        }

        timeline.fit();

        timeline.on('select', function (properties) {
          selectedItem = properties.items[0];
          console.log("Selected Item", selectedItem);
          Shiny.onInputChange("tlSelectEvent", {id:id, items:items.get(selectedItem)});
        });


      },

      resize: function(width, height) {
        // TODO: code to re-render the widget with a new size
      },

      tl: timeline // return instance variable

    };
  }
});

Shiny.addCustomMessageHandler('timeline-select', function(data) {
  var id = data.elid;
  var el = document.getElementById(id);
  var timeline = HTMLWidgets.getInstance(el).tl;
  timeline.setSelection(data.itemid, {focus: true});
});
