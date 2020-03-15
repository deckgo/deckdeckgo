export class ServiceWorkerUtils {
  static async cacheUrls(cacheName: string, urls: string[]): Promise<void> {
    const myCache: Cache = await window.caches.open(cacheName);
    const filteredList: string[] = await this.filterOpaqueRequests(myCache, urls);
    await myCache.addAll(filteredList);
  }

  private static filterOpaqueRequests(cache: Cache, list: string[]): Promise<string[]> {
    return new Promise<string[]>(async (resolve) => {
      if (!cache) {
        resolve([]);
        return;
      }

      if (!list || list.length <= 0) {
        resolve([]);
        return;
      }

      // Elements of the already displayed slides might produce an opaque requests for the SW
      const filterListOpaque = [];

      for (const url of list) {
        const match: Response = await cache.match(url);
        if (!match || match.type !== 'opaque') {
          filterListOpaque.push(url);
        }
      }

      resolve(filterListOpaque);
    });
  }
}
