// ##############################
// ### dragulaSelectR options ###
// ##############################
var dragulaSelectR = {};

dragulaSelectR.options = {
  isContainer: function(el) {
    return el.classList.contains('ds-dragzone');
  },
  copy: function(el, source) {
    // Source -> Target only
    return source.classList.contains('ds-dragzone');
  },
  invalid: function (el, handle) {
    return $(el).hasClass('ds-locked');
  },
  accepts: function(el, target, source, sibling) {
    // Make sure option exists within dropzone
    var dropoption = $(target).children(".ds-dropzone-options").children('.ds-dropoption[data-value="' + $(el).data('value') + '"]');

    // Source -> Target only AND
    //   no dropzone to different dropzone AND (note: caused issue when drop triggered before remove - might change in future)
    //   valid available option in dropzone
    return ((!target.classList.contains('ds-dragzone')) &&
            !(source.classList.contains('ds-dropzone') && (source.id !== target.id)) &&
            (dropoption.length > 0));
  },
  revertOnSpill: true, // Always revert to source container on spill
  removeOnSpill: true  // Always remove drag item on spill
};

// #############################
// ### dragulaSelectR events ###
// #############################
$(document).on("ready", function() {
  dragulaSelectR.drake = dragula(dragulaSelectR.options);

  dragulaSelectR.drake.on("drag", function(el, source) {
    let removeOnSpill = $(source).data('remove-on-spill');
    dragulaSelectR.options.removeOnSpill = (removeOnSpill !== undefined ? removeOnSpill : true);
  });

  dragulaSelectR.drake.on("dragend", function(el) {
    dragulaSelectR.options.removeOnSpill = true; // Set back to default
  });

  dragulaSelectR.drake.on("drop", function(el, target, source, sibling) {
    // Coming in from source - otherwise, do nothing
    if ($(el).hasClass('ds-dragitem')) {
      // Capture number of existing items with this value
      var numitems = $(target).children('[data-value="' + $(el).data('value') + '"]').length;

      // If set to only one per value
      var multivalued = $(target).hasClass('ds-multivalued');
      if (multivalued || ((!multivalued) && (numitems === 1))) {
        // Clone option with corresponding value
        var dropoption = $(target).children(".ds-dropzone-options").children('.ds-dropoption[data-value="' + $(el).data('value') + '"]');
        var $newItem = dropoption.clone();

        // Update dropzone counter
        $(target).data('counter', $(target).data('counter') + 1);

        // Set instance id for new item (only used for multivalued)
        $newItem.attr('data-instance', multivalued ? $(target).data('counter') : '');

        var hidden = $(target).hasClass('ds-hidden');
        if (!hidden && sibling) {
          $newItem.insertBefore(sibling);
        } else {
          $(target).append($newItem);
        }
      }

      // Always remove element coming from source
      el.remove();
    }

    // Raise an event to signal that the value changed
    $(target).trigger("change");
  });

  dragulaSelectR.drake.on("over", function(el, container, source) {
    // Highlighting
    if ($(container).hasClass('ds-highlight')) {
      $(container).addClass('gu-highlight');
    }
    // Set direction
    let direction = $(container).data('direction');
    dragulaSelectR.options.direction = (direction !== undefined ? direction : "vertical");
    // Change content of item in transit
    $(el).html($(container).children(".ds-dropzone-options").children('.ds-dropoption[data-value="' + $(el).data('value') + '"]').html());
  });

  dragulaSelectR.drake.on("out", function(el, container, source) {
    $(container).removeClass('gu-highlight');
    if (!$(source).data('remove-on-spill')) {
      $(el).removeClass("gu-hide");
    }
  });

  // Trigger change on item removal
  dragulaSelectR.drake.on("remove", function(el, container, source) {
    if ($(source).hasClass('ds-dropzone')) {
      $(source).trigger("change");
    }

    let dzId = $(source).attr('id');
    if ($(el).hasClass('ds-selected')) {
      Shiny.onInputChange(dzId + "_selected", null);
    }
    if ($(el).hasClass('ds-invisible')) {
      Shiny.onInputChange(dzId + "_invisible", getValues($(source), '.ds-invisible'));
    }
    if ($(el).hasClass('ds-locked')) {
      Shiny.onInputChange(dzId + "_locked", getValues($(source), '.ds-locked'));
    }
  });
});

// #####################
// ### dropZoneInput ###
// #####################
var dropZoneBinding = new Shiny.InputBinding();

$.extend(dropZoneBinding, {
  find: function(scope) {
    return $(scope).find(".ds-dropzone");
  },
  initialize: function(el) {
    dragulaSelectR.drake.containers.push(el);

    // Set multivalued counter to max instance value
    $(el).data('counter', Math.max(0, ...$('#' + el.id + ' > .ds-dropoption').map(function() { return this.dataset.instance })));

    // Selection is being made on a selectable zone
    if ($(el).hasClass('ds-selectable')) {
      $(el).on("click", ".ds-dropoption", function() {
        let $clicked = $(this);
        let dzId = $clicked.parent().attr('id');
        let newValue = optionValue($clicked.get(0));

        // Get currently selected - right now only one allowed
        let $selected = $clicked.siblings(".ds-selected");
        let currValue = ($selected.length ? optionValue($selected.get(0)) : null);

        $selected.removeClass("ds-selected");
        $clicked.addClass("ds-selected");
        Shiny.onInputChange(dzId + "_selected", newValue);
      });
    }

    // Toggle visibility
    $(el).on("click", ".ds-dropoption > .ds-toggle-visible i", function(ev) {
      ev.stopPropagation(); // Avoid selecting
      $(this).toggleClass("fa-eye fa-eye-slash");
      $(this).closest(".ds-dropoption").toggleClass("ds-invisible");
      let $dropzone = $(this).closest(".ds-dropzone");
      Shiny.onInputChange($dropzone.attr('id') + "_invisible", getValues($dropzone, '.ds-invisible'));
    });

    // Toggle draggability
    $(el).on("click", ".ds-dropoption > .ds-toggle-lock i", function(ev) {
      ev.stopPropagation(); // Avoid selecting
      $(this).toggleClass("fa-lock fa-lock-open");
      $(this).closest(".ds-dropoption").toggleClass("ds-locked");
      let $dropzone = $(this).closest(".ds-dropzone");
      Shiny.onInputChange($dropzone.attr('id') + "_locked", getValues($dropzone, '.ds-locked'));
    });
  },
  getValue: function(el) {
    return $('#' + el.id + ' > .ds-dropoption').map(function() { return optionValue(this) }).get();
  },
  setValue: function(el, options) {
  },
  subscribe: function(el, callback) {
    $(el).on("change.dropZoneBinding", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".dropZoneBinding");
  },
  receiveMessage: function(el, data) {
    if (data.hasOwnProperty('action')) {
      if (data.action === "entangle") {
        // Replace drop options
        $('#' + el.id).children('.ds-dropoption').remove();
        $('#' + data.sourceId).children('.ds-dropoption').clone().appendTo('#' + el.id);

        // Copy counter information
        $('#' + el.id).data('counter', $('#' + data.sourceId).data('counter'));

        $(el).trigger("change");
      }
    }
  }
});

Shiny.inputBindings.register(dropZoneBinding);

// #########################
// ### Helpful functions ###
// #########################
function optionValue(el) {
  return [el.dataset.value, el.dataset.instance].filter(Boolean).join('-ds-');
}

function getValues($dropzone, withClass) {
  return $dropzone.children(withClass).map(function() { return optionValue(this) }).get();
}
