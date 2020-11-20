export function unifyEvent(e: any): any {
  return e.changedTouches ? e.changedTouches[0] : e;
}

export function debounce(func: Function, timeout?: number) {
  let timer: NodeJS.Timer | undefined;

  return (...args: any[]) => {
    const next = () => func(...args);

    if (timer) {
      clearTimeout(timer);
    }

    timer = setTimeout(next, timeout && timeout > 0 ? timeout : 300);
  };
}

export function isMobile(): boolean {
  if (!window) {
    return false;
  }

  return window.matchMedia('(any-pointer:coarse)').matches;
}

export function isIOS(): boolean {
  if (!window || !navigator) {
    return false;
  }

  const a: string = navigator.userAgent || navigator.vendor || (window as any).opera;

  return /iPhone|iPod/i.test(a) || isIPad();
}

export function isIPad(): boolean {
  if (!window || !navigator) {
    return false;
  }

  const a: string = navigator.userAgent || navigator.vendor || (window as any).opera;

  // iOS 12 and below
  if (/iPad/i.test(a)) {
    return true;
  }

  // iOS 13+
  return /Macintosh/i.test(a) && isMobile();
}

export function isFullscreen(): boolean {
  if (!window || !screen) {
    return false;
  }

  return window.innerHeight == screen.height;
}

export function isFirefox(): boolean {
  if (!window || !navigator) {
    return false;
  }

  const a: string = navigator.userAgent || navigator.vendor || (window as any).opera;

  return /firefox/i.test(a);
}

export function isRTL(): boolean {
  if (!document || !document.documentElement) {
    return false;
  }

  const htmlDir: string | null = document.documentElement.getAttribute('dir');
  return htmlDir !== null && htmlDir === 'rtl';
}
