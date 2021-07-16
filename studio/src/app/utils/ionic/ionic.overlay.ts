// Source: https://github.com/ionic-team/ionic-framework/blob/7315e0157b917d2e3586c64f8082026827812943/core/src/utils/overlays.ts

import type {
  ActionSheetOptions,
  AlertOptions,
  HTMLIonOverlayElement,
  LoadingOptions,
  ModalOptions,
  PickerOptions, PopoverOptions, ToastOptions
} from '@ionic/core';

const getAppRoot = (doc: Document) => {
  return doc.querySelector('ion-app') || doc.body;
};

export const getOverlay = (doc: Document, overlayTag?: string, id?: string): HTMLIonOverlayElement | undefined => {
  const overlays = getOverlays(doc, overlayTag);
  return (id === undefined)
    ? overlays[overlays.length - 1]
    : overlays.find(o => o.id === id);
};

const componentOnReady = (el: any, callback: any) => {
  if (el.componentOnReady) {
    el.componentOnReady().then((resolvedEl: any) => callback(resolvedEl));
  } else {
    raf(() => callback(el));
  }
}

declare const __zone_symbol__requestAnimationFrame: any;

const raf = (h: any) => {
  if (typeof __zone_symbol__requestAnimationFrame === 'function') {
    return __zone_symbol__requestAnimationFrame(h);
  }
  if (typeof requestAnimationFrame === 'function') {
    return requestAnimationFrame(h);
  }
  return setTimeout(h);
};

export const createOverlay = <T extends HTMLIonOverlayElement>(tagName: string, opts: object | undefined): Promise<T> => {
  /* tslint:disable-next-line */
  if (typeof customElements !== 'undefined') {
    return customElements.whenDefined(tagName).then(() => {
      const element = document.createElement(tagName) as HTMLIonOverlayElement;
      element.classList.add('overlay-hidden');

      // convert the passed in overlay options into props
      // that get passed down into the new overlay
      Object.assign(element, opts);

      // append the overlay element to the document body
      getAppRoot(document).appendChild(element);

      return new Promise(resolve => componentOnReady(element, resolve));
    });
  }
  return Promise.resolve() as any;
};

export const dismissOverlay = (doc: Document, data: any, role: string | undefined, overlayTag: string, id?: string): Promise<boolean> => {
  const overlay = getOverlay(doc, overlayTag, id);
  if (!overlay) {
    return Promise.reject('overlay does not exist');
  }
  return overlay.dismiss(data, role);
};

export const getOverlays = (doc: Document, selector?: string): HTMLIonOverlayElement[] => {
  if (selector === undefined) {
    selector = 'ion-alert,ion-action-sheet,ion-loading,ion-modal,ion-picker,ion-popover,ion-toast';
  }
  return (Array.from(doc.querySelectorAll(selector)) as HTMLIonOverlayElement[])
    .filter(c => c.overlayIndex > 0);
};

const createController = <Opts extends object, HTMLElm extends any>(tagName: string) => {
  return {
    create(options: Opts): Promise<HTMLElm> {
      return createOverlay(tagName, options) as any;
    },
    dismiss(data?: any, role?: string, id?: string) {
      return dismissOverlay(document, data, role, tagName, id);
    },
    async getTop(): Promise<HTMLElm | undefined> {
      return getOverlay(document, tagName) as any;
    }
  };
};

export const alertController = /*@__PURE__*/createController<AlertOptions, HTMLIonAlertElement>('ion-alert');
export const actionSheetController = /*@__PURE__*/createController<ActionSheetOptions, HTMLIonActionSheetElement>('ion-action-sheet');
export const loadingController = /*@__PURE__*/createController<LoadingOptions, HTMLIonLoadingElement>('ion-loading');
export const modalController = /*@__PURE__*/createController<ModalOptions, HTMLIonModalElement>('ion-modal');
export const pickerController = /*@__PURE__*/createController<PickerOptions, HTMLIonPickerElement>('ion-picker');
export const popoverController = /*@__PURE__*/createController<PopoverOptions, HTMLIonPopoverElement>('ion-popover');
export const toastController = /*@__PURE__*/createController<ToastOptions, HTMLIonToastElement>('ion-toast');
