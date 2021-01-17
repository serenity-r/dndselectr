// #########################
// ### dndselectr events ###
// ###     R <-> JS      ###
// #########################
$(function() {
  initdndselectr();

  // #####################
  // ### dropZoneInput ###
  // #####################
  var dropZoneBinding = new Shiny.InputBinding();

  $.extend(dropZoneBinding, {
    find: function(scope) {
      return $(scope).find(".ds-dropzone");
    },
    initialize: function(el) {
      dndselectr.drake.containers.push(el);

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
      $(el).on("click", ".ds-dropoption .ds-toggle-visible i", function(ev) {
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

        // Store settings to allow for server-side checking in updateDropZoneInput
        Shiny.onInputChange(dzId + "_settings:ds-fix-settings", {
          choices: $(el).find('.ds-dropzone-options > .ds-dropoption').map(function() { return optionValue(this) }).get(),
          multivalued: $(el).hasClass('ds-multivalued'),
          maxInput: $(el).data('max-input')
        });
      });
    },
    getValue: function(el) {
      return $('#' + el.id + ' > .ds-dropoption').map(function() { return optionValue(this) }).get();
    },
    setValue: function(el, value) {
      // Remove drop options
      $(el).children('.ds-dropoption').each(function(index) {
        dndselectr.detach(this, el);
      });

      // Add new drop options
      let i = 0;
      Object.values(value.values).forEach(function(val) {
        $(el).data('counter', $(el).data('counter') + 1);
        let $newItem = $(el).children(".ds-dropzone-options")
          .children('.ds-dropoption[data-value="' + val + '"]')
          .clone()
          .attr("data-instance", $(el).hasClass('ds-multivalued') ? $(el).data('counter') : null)
          .addClass('locked' in value ? (Array.isArray(value.locked) ? value.locked[i] : value.locked) : null)
          .addClass('selected' in value ? (Array.isArray(value.selected) ? value.selected[i] : value.selected) : null)
          .addClass('invisible' in value ? (Array.isArray(value.invisible) ? value.invisible[i] : value.invisible) : null)
          .appendTo(el);

        // Call server to update UI if appropriate -- duplicate code from append function
        if ($(el).data('server')) {
          $newItem.empty();
          let dzId = $(el).attr('id');
          Shiny.onInputChange(dzId + "_server",
            {
              value: optionValue($newItem[0]),
              selector: "#" + dzId + " > .ds-dropoption[data-value='" + val + "']" + ($(el).hasClass('ds-multivalued') ? "[data-instance=" + $(el).data('counter') + "]" : ""),
              nonce: Math.random()
            });
        }

        i++;
      });

      // Update Shiny variables
      let dzId = el.id;
      Shiny.onInputChange(dzId + "_selected", getValues($(el), '.ds-selected'));
      Shiny.onInputChange(dzId + "_invisible", getValues($(el), '.ds-invisible'));
      Shiny.onInputChange(dzId + "_locked", getValues($(el), '.ds-locked'));

      // Toggle placeholder status
      let numItemsTotal = Object.values(value.values).length;
      if (numItemsTotal === 0) {
        $(el).children(".ds-placeholder").removeClass("hidden");
      } else {
        $(el).children(".ds-placeholder").addClass("hidden");
      }

      // Add proper class if at maximum input
      if (numItemsTotal === Number($(el).data('maxInput'))) {
        $(el).addClass('ds-max-input');
      } else {
        $(el).removeClass('ds-max-input');
      }
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
        } else
        if (data.action === "append") {
          dndselectr.append(data.value, el, null);
          $(el).trigger("change");
        } else
        if (data.action === "select") {
          dndselectr.unselect(el);
          let selector = ".ds-dropoption[data-value='" + data.val + "']" + (data.hasOwnProperty('id') ? "[data-instance='" + data.id + "']" : "");
          $(el).children(selector).trigger("click");
        } else
        if (data.action === "unselect") {
          dndselectr.unselect(el);
        } else
        if (data.action === "remove_selected") {
          dndselectr.detach($(el).children(".ds-selected")[0], el);

          // Possibly show placeholder
          let numItemsTotal = $(el).children('.ds-dropoption:not(".gu-transit")').length;
          if (numItemsTotal === 0) {
            $(el).children(".ds-placeholder").removeClass('hidden');
          }

          $(el).trigger("change");
        }
      } // END: action

      if (data.hasOwnProperty('placeholder')) {
        $(el).children(".ds-placeholder").html(data.placeholder);

        // Un-hide placeholder if no items in non-hidden dropzone
        if (!$(el).hasClass("ds-hidden") && ($(el).children('.ds-dropoption:not(".gu-transit")').length === 0)) {
          $(el).children(".ds-placeholder").removeClass("hidden");
        }
      }

      // Only need to update Shiny input storing dropzone settings
      if (data.hasOwnProperty('choices')) {
        setTimeout(function() {
          Shiny.onInputChange(el.id + "_settings:ds-fix-settings", {
            choices: $(el).find('.ds-dropzone-options > .ds-dropoption').map(function() { return optionValue(this) }).get(),
            multivalued: $(el).hasClass('ds-multivalued'),
            maxInput: $(el).data('max-input')
          });
        });
      }

      if (data.hasOwnProperty('presets')) {
        // Gonna keep this double if just in case we refactor to include other options (like selected)
        if (data.presets.hasOwnProperty('values')) {
          this.setValue(el, data.presets);

          $(el).trigger("change");
        }
      }
    }
  });

  Shiny.inputBindings.register(dropZoneBinding, "dndselectr.dropZoneBinding");
});

// ##########################
// ### dndselectr options ###
// ###  Defines behavior  ###
// ##########################
var dndselectr = {
  options: {
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
    moves: function (el, container, handle) {
      return !$(el).has('.ds-handle').length || handle.classList.contains('ds-handle');
    },
    accepts: function(el, target, source, sibling) {
      // Make sure option exists within dropzone
      let validOption = $(target).children(".ds-dropzone-options").children('.ds-dropoption[data-value="' + $(el).data('value') + '"]').length > 0;

      // Check if item is already there (not including temporary
      //   in-transit pre-drop item)
      let numItemsWithValue = $(target).children('[data-value="' + $(el).data('value') + '"]:not(".gu-transit")').length;
      let multivalued = $(target).hasClass('ds-multivalued');
      let spaceAvailable = ((multivalued || (numItemsWithValue === 0)) &&
                            (!$(target).hasClass('ds-max-input') ||
                             $(el).hasClass('ds-dropoption') ||
                             ($(target).hasClass('ds-replace-on-drop') && (sibling !== null))));

      // Source -> Target only AND
      //   no dropzone to different dropzone AND (note: caused issue when drop triggered before remove - might change in future)
      //   valid available option in dropzone AND
      //   not before a frozen item (note: need .gu-transit check as well)
      return (!target.classList.contains('ds-dragzone') &&
              !(source.classList.contains('ds-dropzone') && (source.id !== target.id)) &&
              validOption && spaceAvailable &&
              !$(sibling).is('.ds-freeze') && !$('.gu-transit').next().is('.ds-freeze'));
    },
    revertOnSpill: true, // Always revert to source container on spill
    removeOnSpill: true  // Always remove drag item on spill
  },
  append: function(value, target, sibling) {
    // target = dropzone; sibling = dropitem (or null for end)
    // If calling manually, make sure you call $(target).trigger("change");

    // Clone option with corresponding value
    let $newItem = $(target).children(".ds-dropzone-options").children('.ds-dropoption[data-value="' + value + '"]').clone();

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
      $newItem.trigger("click");
    }

    // If replace on drop, remove replaced element (will always be next)
    if ($(target).hasClass('ds-max-input') && $(target).hasClass('ds-replace-on-drop')) {
      if (!$(target).hasClass('ds-hidden') && sibling) {
        sibling.remove();
      } else {
        $(target).children('.ds-dropoption').last().prev().remove();
      }
    }

    // Check if at max allowable inputs
    let numItemsTotal = $(target).children('.ds-dropoption:not(".gu-transit")').length;
    if (numItemsTotal === Number($(target).data('maxInput'))) {
      $(target).addClass('ds-max-input');
    }

    // Hide placeholder
    $(target).children(".ds-placeholder").addClass("hidden");

    // Call server to update UI if appropriate
    if ($(target).data('server')) {
      $newItem.empty();
      let dzId = $(target).attr('id');
      Shiny.onInputChange(dzId + "_server",
        {
          value: optionValue($newItem[0]),
          selector: "#" + dzId + " > .ds-dropoption[data-value='" + value + "']" + ($(target).hasClass('ds-multivalued') ? "[data-instance='" + $(target).data('counter') + "']" : ""),
          nonce: Math.random()
        });
    }
  },
  detach: function(el, container) {
    // Note: This function detaches the element from the DOM (if attached) and
    //   takes care of necessary processing of the dropzone input.  Returns the
    //   detached jQuery element.

    // First, detach if necessary
    el = (jQuery.contains(document, el) ? $(el).detach()[0] : el);

    $(container).removeClass('ds-max-input');

    let dzId = $(container).attr('id');
    if ($(el).hasClass('ds-selected')) {
      Shiny.onInputChange(dzId + "_selected", null);
    }
    if ($(el).hasClass('ds-invisible')) {
      Shiny.onInputChange(dzId + "_invisible", getValues($(container), '.ds-invisible'));
    }
    if ($(el).hasClass('ds-locked')) {
      Shiny.onInputChange(dzId + "_locked", getValues($(container), '.ds-locked'));
    }

    if ($(container).data('server')) {
      Shiny.unbindAll(el);
    }

    return($(el));
  },
  unselect: function(container) {
    $(container).children().removeClass("ds-selected");
    Shiny.onInputChange(container.id + "_selected", null);
  }
};

// #############################
// ### Initialize dndselectr ###
// #############################
function initdndselectr() {
  dndselectr.drake = dragula(dndselectr.options);

  dndselectr.drake.on("drag", function(el, source) {
    // Set removeOnSpill from source zone setting
    let removeOnSpill = $(source).data('remove-on-spill');
    dndselectr.options.removeOnSpill = (removeOnSpill !== undefined ? removeOnSpill : true);
  });

  dndselectr.drake.on("dragend", function(el) {
    // Set removeOnSpill back to default
    dndselectr.options.removeOnSpill = true;
  });

  dndselectr.drake.on("drop", function(el, target, source, sibling) {
    // Coming in from source - otherwise, do nothing
    if ($(el).hasClass('ds-dragitem')) {
      dndselectr.append($(el).data('value'), target, sibling);

      // Always remove element coming from source
      el.remove();
    }

    // Raise an event to signal that the value changed
    $(target).trigger("change");
  });

  dndselectr.drake.on("over", function(el, container, source) {
    $(container).addClass('gu-dragover');

    // Set direction (timed out due to glitch when over first occurs)
    setTimeout(function() {
      let direction = $(container).data('direction');
      dndselectr.options.direction = (direction !== undefined ? direction : "vertical");
    });
    // Change content of item in transit (only if drag -> drop)
    if ($(el).hasClass('ds-dragitem')) {
      $(el).html($(container).children(".ds-dropzone-options").children('.ds-dropoption[data-value="' + $(el).data('value') + '"]').html());
    }

    // Hide placeholder if dropzone not hidden
    if (!$(container).hasClass("ds-hidden")) {
      $(container).children(".ds-placeholder").addClass("hidden");
    }
  });

  dndselectr.drake.on("out", function(el, container, source) {
    $(container).removeClass('gu-dragover');

    // New entry. Dragula doesn't remove temporary gu-hide class for
    //   some reason.
    if (!$(source).data('remove-on-spill')) {
      $(el).removeClass("gu-hide");
    }

    // Un-hide placeholder if no more items in non-hidden dropzone
    if (!$(container).hasClass("ds-hidden") && ($(container).children('.ds-dropoption:not(".gu-transit")').length === 0)) {
      $(container).children(".ds-placeholder").removeClass("hidden");
    }
  });

  // Trigger changes on item removal
  dndselectr.drake.on("remove", function(el, container, source) {
    dndselectr.detach(el, container);

    if ($(source).hasClass('ds-dropzone')) {
      $(source).trigger("change");
    }
  });
}

// #########################
// ### Helpful functions ###
// #########################
function optionValue(el) {
  return [el.dataset.value, el.dataset.instance].filter(Boolean).join('-ds-');
}

function getValues($dropzone, withClass) {
  return $dropzone.children(withClass).map(function() { return optionValue(this) }).get();
}
