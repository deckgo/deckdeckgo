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

  const a: string = userAgent();

  return /iPhone|iPod/i.test(a) || isIPad();
}

export function isIPad(): boolean {
  if (!window || !navigator) {
    return false;
  }

  const a: string = userAgent();

  // iOS 12 and below
  if (/iPad/i.test(a)) {
    return true;
  }

  // iOS 13+
  return /Macintosh/i.test(a) && isMobile();
}

export function isFullscreen(): boolean {
  return (
    // @ts-ignore
    // prettier-ignore
    !!(document.fullscreenElement || document.mozFullScreenElement || document.webkitFullscreenElement || document.msFullscreenElement)
  );
}

export function isFirefox(): boolean {
  if (!window || !navigator) {
    return false;
  }

  const a: string = userAgent();

  return /firefox/i.test(a);
}

export function isRTL(): boolean {
  const htmlDir: string | null = document.documentElement.getAttribute('dir');
  return htmlDir !== null && htmlDir === 'rtl';
}

// Source: Ionic ionic-framework/angular/src/providers/platform.ts

export function isLandscape(): boolean {
  return !isPortrait();
}

export function isPortrait(): boolean {
  if (!window) {
    return false;
  }

  return window.matchMedia && window.matchMedia('(orientation: portrait)').matches;
}

// Source: Ionic ionic-framework/core/src/utils/platform.ts

export const isAndroid = (): boolean => {
  if (!window) {
    return false;
  }

  const a: string = userAgent();

  return /android|sink/i.test(a);
};

export const isAndroidTablet = (): boolean => {
  if (!window) {
    return false;
  }

  const a: string = userAgent();

  return isAndroid() && !/mobile/i.test(a);
};

const userAgent = (): string => {
  return navigator.userAgent || navigator.vendor || (window as any).opera;
};
