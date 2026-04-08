Shiny.addCustomMessageHandler('activate-tab', function(message) {
    try {
      var container = document.getElementById(message.id);
      if (!container) return;
      // try common data-attribute selectors used by bslib/tabsets
      var selectorCandidates = [
        '[data-value=\"' + message.value + '\"]',
        'a[data-value=\"' + message.value + '\"]',
        'button[data-value=\"' + message.value + '\"]',
        'a[data-bs-target*=\"' + message.value + '\"]',
        'button[data-bs-target*=\"' + message.value + '\"]',
        'a[role=\"tab\"][href*=\"#\"][href*=\"' + message.value + '\"]'
      ];
      var el = null;
      for (var i = 0; i < selectorCandidates.length; i++) {
        el = container.querySelector(selectorCandidates[i]);
        if (el) break;
      }
      // fallback: search by visible text match (not ideal but helpful)
      if (!el) {
        var links = container.querySelectorAll('a, button, [role=\"tab\"]');
        for (var i = 0; i < links.length; i++) {
          if (links[i].textContent && links[i].textContent.trim().toLowerCase() === message.value.toLowerCase()) {
            el = links[i];
            break;
          }
        }
      }
      if (el) {
        // simulate a user click
        el.click();
      } else {
        console.warn('activate-tab: no element found for', message);
      }
    } catch(e) {
      console.error('activate-tab handler error', e);
    }
  });
