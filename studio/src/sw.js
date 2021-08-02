/**
 * Custom service worker as displayed in the Stencil doc
 * https://stenciljs.com/docs/service-workers
 */

importScripts('https://storage.googleapis.com/workbox-cdn/releases/5.1.2/workbox-sw.js');

self.addEventListener('message', (event) => {
  if (event.data && event.data.type === 'SKIP_WAITING') {
    self.skipWaiting();
  }
});

workbox.setConfig({debug: false});

const {CacheableResponsePlugin} = workbox.cacheableResponse;
const {ExpirationPlugin} = workbox.expiration;

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
      new CacheableResponsePlugin({
        statuses: [0, 200]
      }),
      new ExpirationPlugin({
        maxAgeSeconds: 60 * 60 * 24 * 365,
        maxEntries: 30
      })
    ]
  })
);

// Cache the images
workbox.routing.registerRoute(
  /^(?!.*(?:unsplash|giphy|tenor|firebasestorage))(?=.*(?:png|jpg|jpeg|svg|webp|gif)).*/,
  new workbox.strategies.CacheFirst({
    cacheName: 'images',
    plugins: [
      new ExpirationPlugin({
        maxAgeSeconds: 30 * 24 * 60 * 60,
        maxEntries: 60
      })
    ]
  })
);

workbox.routing.registerRoute(
  /^(?=.*(?:unsplash|giphy|tenor|firebasestorage))(?=.*(?:png|jpg|jpeg|svg|webp|gif)).*/,
  new workbox.strategies.StaleWhileRevalidate({
    cacheName: 'cors-images',
    plugins: [
      new ExpirationPlugin({
        maxAgeSeconds: 30 * 24 * 60 * 60,
        maxEntries: 60
      }),
      new CacheableResponsePlugin({
        statuses: [0, 200]
      })
    ]
  })
);

// Cache the data
workbox.routing.registerRoute(
  /^(?=.*(?:githubusercontent|firebasestorage))(?=.*(?:csv|json)).*/,
  new workbox.strategies.StaleWhileRevalidate({
    cacheName: 'data-content',
    plugins: [
      new ExpirationPlugin({
        maxAgeSeconds: 30 * 24 * 60 * 60,
        maxEntries: 60
      })
    ]
  })
);

// Cache unpkg notably for language definitions
workbox.routing.registerRoute(
  /^(?=.*(unpkg\.com|cdnjs\.cloudflare\.com|cdn\.jsdelivr\.net)).*/,
  new workbox.strategies.StaleWhileRevalidate({
    cacheName: 'unpkg',
    plugins: [
      new ExpirationPlugin({
        maxAgeSeconds: 30 * 24 * 60 * 60,
        maxEntries: 60
      })
    ]
  })
);

workbox.routing.registerRoute(
  /^(?=.*deckdeckgo\.com).*/,
  new workbox.strategies.NetworkFirst({
    cacheName: 'editor'
  })
);

// the precache manifest will be injected into the following line
self.workbox.precaching.precacheAndRoute(self.__WB_MANIFEST, {
  // Ignore all URL parameters otherwise /editor/:id won't be cached and therefore not accessible directly offline
  ignoreURLParametersMatching: [/.*/]
});
