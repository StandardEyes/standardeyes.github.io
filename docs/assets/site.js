// StandardEyes website: mobile menu, image lightbox, table-of-contents highlight, copy buttons.
(function () {
  var toggle = document.querySelector('.navtoggle'), nav = document.getElementById('sitenav');
  if (toggle && nav) {
    var setOpen = function (open) {
      nav.classList.toggle('open', open);
      toggle.setAttribute('aria-expanded', open ? 'true' : 'false');
    };
    toggle.addEventListener('click', function () { setOpen(!nav.classList.contains('open')); });
    nav.addEventListener('click', function (e) { if (e.target.closest('a')) setOpen(false); });
    document.addEventListener('click', function (e) {
      if (nav.classList.contains('open') && !nav.contains(e.target) && !toggle.contains(e.target)) setOpen(false);
    });
    document.addEventListener('keydown', function (e) {
      if (e.key === 'Escape' && nav.classList.contains('open')) { setOpen(false); toggle.focus(); }
    });
  }

  var box = document.getElementById('lightbox');
  if (box) {
    var img = box.querySelector('img'), cap = box.querySelector('figcaption'), closeBtn = box.querySelector('.close'), last = null;
    var behind = function (off) {
      document.querySelectorAll('header.site, main, footer.site').forEach(function (el) { el.inert = off; });
    };
    var close = function () {
      box.hidden = true; img.removeAttribute('src'); document.body.style.overflow = '';
      behind(false); if (last) last.focus();
    };
    document.querySelectorAll('.zoom').forEach(function (b) {
      b.addEventListener('click', function () {
        var im = b.querySelector('img');
        img.src = b.getAttribute('data-full') || im.src; img.alt = im.alt;
        cap.textContent = b.getAttribute('data-caption') || im.alt;
        last = b; box.hidden = false; document.body.style.overflow = 'hidden'; behind(true); closeBtn.focus();
      });
    });
    box.addEventListener('click', function (e) { if (e.target === box || e.target === closeBtn) close(); });
    document.addEventListener('keydown', function (e) {
      if (box.hidden) return;
      if (e.key === 'Escape') close();
      else if (e.key === 'Tab') { e.preventDefault(); closeBtn.focus(); }   // the close button is the only control
    });
  }

  // one video at a time
  var vids = document.querySelectorAll('video');
  vids.forEach(function (v) {
    v.addEventListener('play', function () { vids.forEach(function (o) { if (o !== v) o.pause(); }); });
  });

  // highlight the section being read in the side table of contents
  var toc = document.querySelector('.toc');
  if (toc && 'IntersectionObserver' in window) {
    var links = {};
    toc.querySelectorAll('a[href^="#"]').forEach(function (a) { links[a.getAttribute('href').slice(1)] = a; });
    var io = new IntersectionObserver(function (es) {
      es.forEach(function (e) {
        if (e.isIntersecting && links[e.target.id]) {
          Object.keys(links).forEach(function (k) { links[k].classList.remove('on'); });
          links[e.target.id].classList.add('on');
        }
      });
    }, { rootMargin: '-90px 0px -70% 0px' });
    Object.keys(links).forEach(function (id) { var el = document.getElementById(id); if (el) io.observe(el); });
  }

  document.querySelectorAll('[data-copy]').forEach(function (b) {
    b.setAttribute('aria-live', 'polite');
    b.addEventListener('click', function () {
      var t = document.querySelector(b.getAttribute('data-copy')), old = b.textContent;
      var say = function (msg) { b.textContent = msg; setTimeout(function () { b.textContent = old; }, 2200); };
      if (!t || !navigator.clipboard) { say('Select the text and press Ctrl+C'); return; }
      navigator.clipboard.writeText(t.textContent.trim()).then(function () { say('Copied'); },
        function () { say('Select the text and press Ctrl+C'); });
    });
  });
})();
