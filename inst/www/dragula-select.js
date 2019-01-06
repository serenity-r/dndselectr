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
    return ($(el).hasClass('ds-locked') || $(el).hasClass('ds-placeholder'));
  },
  accepts: function(el, target, source, sibling) {
    // Make sure option exists within dropzone
    let validOption = $(target).children(".ds-dropzone-options").children('.ds-dropoption[data-value="' + $(el).data('value') + '"]').length > 0;

    // Check if item is already there (not including temporary
    //   in-transit pre-drop item)
    let numitems = $(target).children('[data-value="' + $(el).data('value') + '"]:not(".gu-transit")').length;
    let multivalued = $(target).hasClass('ds-multivalued');
    let roomForGrowth = (multivalued || ((!multivalued) && (numitems === 0)));

    // Source -> Target only AND
    //   no dropzone to different dropzone AND (note: caused issue when drop triggered before remove - might change in future)
    //   valid available option in dropzone AND
    //   not before a frozen item (note: need .gu-transit check as well)
    return (!target.classList.contains('ds-dragzone') &&
            !(source.classList.contains('ds-dropzone') && (source.id !== target.id)) &&
            validOption && roomForGrowth &&
            !$(sibling).is('.ds-freeze') && !$('.gu-transit').next().is('.ds-freeze'));
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
    // Set removeOnSpill from source zone setting
    let removeOnSpill = $(source).data('remove-on-spill');
    dragulaSelectR.options.removeOnSpill = (removeOnSpill !== undefined ? removeOnSpill : true);
  });

  dragulaSelectR.drake.on("dragend", function(el) {
    // Set removeOnSpill back to default
    dragulaSelectR.options.removeOnSpill = true;
  });

  dragulaSelectR.drake.on("drop", function(el, target, source, sibling) {
    // Coming in from source - otherwise, do nothing
    if ($(el).hasClass('ds-dragitem')) {
      // Clone option with corresponding value
      let $newItem = $(target).children(".ds-dropzone-options").children('.ds-dropoption[data-value="' + $(el).data('value') + '"]').clone();

      // Update dropzone counter
      $(target).data('counter', $(target).data('counter') + 1);

      // Set instance id for new item (only used for multivalued)
      $newItem.attr('data-instance', $(target).hasClass('ds-multivalued') ? $(target).data('counter') : '');

      if (!$(target).hasClass('ds-hidden') && sibling) {
        $newItem.insertBefore(sibling);
      } else {
        $(target).append($newItem);
      }

      // Trigger selection if applicable
      if ($(target).data('select-on-drop')) {
        console.log("I'm gonna select it!");
        $newItem.trigger("click");
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
    // Set direction (timed out due to glitch when over first occurs)
    setTimeout(function() {
      let direction = $(container).data('direction');
      dragulaSelectR.options.direction = (direction !== undefined ? direction : "vertical");
    });
    // Change content of item in transit
    $(el).html($(container).children(".ds-dropzone-options").children('.ds-dropoption[data-value="' + $(el).data('value') + '"]').html());
  });

  dragulaSelectR.drake.on("out", function(el, container, source) {
    $(container).removeClass('gu-highlight');

    // New entry. Dragula doesn't remove temporary gu-hide class for
    //   some reason.
    if (!$(source).data('remove-on-spill')) {
      $(el).removeClass("gu-hide");
    }
  });

  // Trigger changes on item removal
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

    // Initialize secondary inputs (selected, invisible, and locked).
    //   Need timeout so other binding stuff can happen before we update
    //   secondary inputs (essentially, without the timeout, no dice...)
    setTimeout(function() {
      let dzId = el.id;
      Shiny.onInputChange(dzId + "_selected", getValues($(el), '.ds-selected'));
      Shiny.onInputChange(dzId + "_invisible", getValues($(el), '.ds-invisible'));
      Shiny.onInputChange(dzId + "_locked", getValues($(el), '.ds-locked'));
    });
  },
  getValue: function(el) {
    return $('#' + el.id + ' > .ds-dropoption').map(function() { return optionValue(this) }).get();
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
