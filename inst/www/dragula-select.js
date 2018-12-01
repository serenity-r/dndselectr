$(document).on("ready", function() {
  var drake = dragula({
    isContainer: function(el) {
      return el.classList.contains('ds-dragzone');
    },
    copy: function(el, source) {
      // Source -> Target only
      return [...document.getElementsByClassName('ds-dragzone')].includes(source)
    },
    accepts: function(el, target) {
      // Make sure option exists within dropzone
      var dropoption = $(target).children(".ds-dropzone-options").children(".ds-dropoption[data-value=" + $(el).data('value') + "]");

      // Source -> Target only AND valid available option in dropzone
      return ((![...document.getElementsByClassName('ds-dragzone')].includes(target)) &&
              (dropoption.length > 0))
    },
    revertOnSpill: true, // Always revert to source container on spill
    removeOnSpill: true  // Always remove drag item on spill
  });

  drake.on("drop", function(el, target, source, sibling) {
    // Coming in from source - otherwise, do nothing
    if ($(el).hasClass('ds-dragitem')) {
      // Capture number of existing items with this value
      var numitems = $(target).children("[data-value=" + $(el).data('value') + "]").length;

      // If set to only one per value
      var multivalued = $(target).hasClass('ds-multivalued');
      if (multivalued || ((!multivalued) && (numitems === 1))) {
        // Clone option with corresponding value
        var dropoption = $(target).children(".ds-dropzone-options").children(".ds-dropoption[data-value=" + $(el).data('value') + "]");
        var $newItem = dropoption.clone();
        if (sibling) {
          $newItem.insertBefore(sibling);
        } else {
          $(target).append($newItem);
        }
        el.parentNode.replaceChild($newItem[0], el);
      } else {
        // Always remove element coming from source
        this.remove();
      }
    }
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

  drake.containers.push(document.querySelector('#dropzone'));
});
