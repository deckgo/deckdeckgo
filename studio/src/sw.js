/**
 * Custom service worker as displayed in the Stencil doc
 * https://stenciljs.com/docs/service-workers
 */

importScripts('https://storage.googleapis.com/workbox-cdn/releases/4.3.1/workbox-sw.js');

self.addEventListener('message', (event) => {
  if (event.data && event.data.type === 'SKIP_WAITING') {
    self.skipWaiting();
  }
});

/**
 * Cache font as displayed in the Workbox common recipe
 * https://developers.google.com/web/tools/workbox/guides/common-recipes
 */

// Cache the Google Fonts stylesheets with a stale-while-revalidate strategy.
workbox.routing.registerRoute(
  /^https:\/\/fonts\.googleapis\.com/,
  new workbox.strategies.StaleWhileRevalidate({
    cacheName: 'google-fonts-stylesheets'
  })
);

// Cache the underlying font files with a cache-first strategy for 1 year.
workbox.routing.registerRoute(
  /^https:\/\/fonts\.gstatic\.com/,
  new workbox.strategies.CacheFirst({
    cacheName: 'google-fonts-webfonts',
    plugins: [
      new workbox.cacheableResponse.Plugin({
        statuses: [0, 200]
      }),
      new workbox.expiration.Plugin({
        maxAgeSeconds: 60 * 60 * 24 * 365,
        maxEntries: 30
      })
    ]
  })
);

// Cache the images with a cache-first strategy for 30 days.
workbox.routing.registerRoute(
  /^(?!.*(unsplash|giphy))(?=.*(?:png|jpg|jpeg|svg|webp|gif)).*/,
  new workbox.strategies.CacheFirst({
    cacheName: 'images',
    plugins: [
      new workbox.expiration.Plugin({
        maxAgeSeconds: 30 * 24 * 60 * 60,
        maxEntries: 60
      })
    ]
  })
);

workbox.routing.registerRoute(
  /^(?=.*(unsplash|giphy))(?=.*(?:png|jpg|jpeg|svg|webp|gif)).*/,
  new workbox.strategies.StaleWhileRevalidate({
    cacheName: 'cors-images',
    plugins: [
      new workbox.expiration.Plugin({
        maxAgeSeconds: 30 * 24 * 60 * 60,
        maxEntries: 60
      })
    ]
  })
);

workbox.routing.registerRoute(
  /^(?=.*githubusercontent)(?=.*(?:csv|json)).*/,
  new workbox.strategies.StaleWhileRevalidate({
    cacheName: 'github-content',
    plugins: [
      new workbox.expiration.Plugin({
        maxAgeSeconds: 30 * 24 * 60 * 60,
        maxEntries: 60
      })
    ]
  })
);

// the precache manifest will be injected into the following line
self.workbox.precaching.precacheAndRoute([]);
