var dropZoneBinding = new Shiny.InputBinding();

function optionValue(el) {
  return [el.dataset.value, el.dataset.instance].filter(Boolean).join('-ds-');
}

$.extend(dropZoneBinding, {
  find: function(scope) {
    return $(scope).find(".ds-dropzone");
  },
  initialize: function(el) {
    drake.containers.push(el);

    // Set multivalued counter to max instance value
    $(el).data('counter', Math.max(0, ...$('#' + el.id + ' > .ds-dropoption').map(function() { return this.dataset.instance })));

    // Selection is being made on a selectable zone
    if ($(el).hasClass('ds-selectable')) {
      $(el).on("click", ".ds-dropoption", function() {
        let $clicked = $(this);
        let dzId = $clicked.parent().attr('id');
        let newValue = optionValue($clicked.get(0));

        // Get currently selected - right now only one allowed
        let $selected = $clicked.siblings(".selected");
        let currValue = ($selected.length ? optionValue($selected.get(0)) : null);

        $selected.removeClass("selected");
        $clicked.addClass("selected");
        Shiny.onInputChange(dzId + "_selected", newValue);
      });
    }

    // Toggle visibility
    $(el).on("click", ".ds-dropoption > .visible i", function(ev) {
      ev.stopPropagation(); // Avoid selecting
      $(this).toggleClass("fa-eye fa-eye-slash");
      $(this).closest(".ds-dropoption").toggleClass("inactive");
      $(this).closest(".ds-dropzone").trigger("change");
    });
  },
  getValue: function(el) {
    return $('#' + el.id + ' > .ds-dropoption:not(.inactive)').map(function() { return optionValue(this) }).get();
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

$(document).on("ready", function() {
  drake = dragula({
    isContainer: function(el) {
      return el.classList.contains('ds-dragzone');
    },
    copy: function(el, source) {
      // Source -> Target only
      return source.classList.contains('ds-dragzone');
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
  });

  drake.on("drop", function(el, target, source, sibling) {
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

  // Highlighting
  drake.on("over", function(el, container, source) {
    if ($(container).hasClass('ds-highlight')) {
      $(container).addClass('gu-highlight');
    }
  });
  drake.on("out", function(el, container, source) {
    $(container).removeClass('gu-highlight');
  });

  // Trigger change on item removal
  drake.on("remove", function(el, container, source) {
    if ($(source).hasClass('ds-dropzone')) {
      $(source).trigger("change");
    }

    if (($(source).hasClass('ds-selectable')) &&
        ($(el).hasClass('selected'))) {
      let dzId = $(source).attr('id');
      Shiny.onInputChange(dzId + "_selected", null);
    }
  });
});
