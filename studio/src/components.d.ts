/* tslint:disable */
/**
 * This is an autogenerated file created by the Stencil compiler.
 * It contains typing information for all components that exist in this project.
 */


import { HTMLStencilElement, JSXBase } from '@stencil/core/internal';
import {
  EventEmitter,
} from '@stencil/core';


export namespace Components {
  interface AppAbout {}
  interface AppAddSlideAction {}
  interface AppAvatar {
    'src': string;
  }
  interface AppCode {
    'codeDidChange': EventEmitter<HTMLElement>;
    'selectedElement': HTMLElement;
  }
  interface AppContact {}
  interface AppDeckOrSlide {}
  interface AppDemo {}
  interface AppDeveloper {}
  interface AppEditor {
    'deckId': string;
  }
  interface AppEditorActions {}
  interface AppEditorToolbar {
    'blurSelectedElement': () => Promise<void>;
    'hideToolbar': () => Promise<void>;
    'touch': (element: HTMLElement) => Promise<void>;
    'unSelect': () => Promise<void>;
  }
  interface AppFeed {}
  interface AppFeedCard {
    'author': string;
    'caption': string;
    'compact': boolean;
    'description': string;
    'miniature': boolean;
    'publication': Date;
  }
  interface AppFeedCardTags {
    'editable': boolean;
    'tags': string[];
  }
  interface AppFooter {}
  interface AppGif {}
  interface AppHelp {}
  interface AppHome {}
  interface AppImage {
    'deckOrSlide': boolean;
  }
  interface AppImageColumns {
    'imagesEven': (UnsplashPhoto | TenorGif)[];
    'imagesOdd': (UnsplashPhoto | TenorGif)[];
  }
  interface AppLogo {}
  interface AppMenu {}
  interface AppNavigation {
    'logo': boolean;
    'menuToggle': boolean;
    'presentation': boolean;
    'publish': boolean;
    'user': boolean;
  }
  interface AppNavigationActions {
    'presentation': boolean;
    'publish': boolean;
    'signIn': boolean;
  }
  interface AppOpensource {}
  interface AppPhoto {}
  interface AppPopular {}
  interface AppPrivacy {}
  interface AppPublish {
    'description': string;
  }
  interface AppRemote {}
  interface AppRoot {}
  interface AppServices {}
  interface AppSettings {}
  interface AppSignin {
    'redirect': string;
    'redirectId': string;
  }
  interface AppSlideNavigate {
    'slides': string[];
  }
  interface AppSlideType {}
  interface AppSlotType {
    'selectedElement': HTMLElement;
  }
  interface AppTeam {}
  interface AppTerms {}
  interface AppUserDelete {
    'username': string;
  }
  interface AppUserInfo {
    'avatarColSize': number;
  }
  interface AppUserMenu {}
}

declare global {


  interface HTMLAppAboutElement extends Components.AppAbout, HTMLStencilElement {}
  var HTMLAppAboutElement: {
    prototype: HTMLAppAboutElement;
    new (): HTMLAppAboutElement;
  };

  interface HTMLAppAddSlideActionElement extends Components.AppAddSlideAction, HTMLStencilElement {}
  var HTMLAppAddSlideActionElement: {
    prototype: HTMLAppAddSlideActionElement;
    new (): HTMLAppAddSlideActionElement;
  };

  interface HTMLAppAvatarElement extends Components.AppAvatar, HTMLStencilElement {}
  var HTMLAppAvatarElement: {
    prototype: HTMLAppAvatarElement;
    new (): HTMLAppAvatarElement;
  };

  interface HTMLAppCodeElement extends Components.AppCode, HTMLStencilElement {}
  var HTMLAppCodeElement: {
    prototype: HTMLAppCodeElement;
    new (): HTMLAppCodeElement;
  };

  interface HTMLAppContactElement extends Components.AppContact, HTMLStencilElement {}
  var HTMLAppContactElement: {
    prototype: HTMLAppContactElement;
    new (): HTMLAppContactElement;
  };

  interface HTMLAppDeckOrSlideElement extends Components.AppDeckOrSlide, HTMLStencilElement {}
  var HTMLAppDeckOrSlideElement: {
    prototype: HTMLAppDeckOrSlideElement;
    new (): HTMLAppDeckOrSlideElement;
  };

  interface HTMLAppDemoElement extends Components.AppDemo, HTMLStencilElement {}
  var HTMLAppDemoElement: {
    prototype: HTMLAppDemoElement;
    new (): HTMLAppDemoElement;
  };

  interface HTMLAppDeveloperElement extends Components.AppDeveloper, HTMLStencilElement {}
  var HTMLAppDeveloperElement: {
    prototype: HTMLAppDeveloperElement;
    new (): HTMLAppDeveloperElement;
  };

  interface HTMLAppEditorElement extends Components.AppEditor, HTMLStencilElement {}
  var HTMLAppEditorElement: {
    prototype: HTMLAppEditorElement;
    new (): HTMLAppEditorElement;
  };

  interface HTMLAppEditorActionsElement extends Components.AppEditorActions, HTMLStencilElement {}
  var HTMLAppEditorActionsElement: {
    prototype: HTMLAppEditorActionsElement;
    new (): HTMLAppEditorActionsElement;
  };

  interface HTMLAppEditorToolbarElement extends Components.AppEditorToolbar, HTMLStencilElement {}
  var HTMLAppEditorToolbarElement: {
    prototype: HTMLAppEditorToolbarElement;
    new (): HTMLAppEditorToolbarElement;
  };

  interface HTMLAppFeedElement extends Components.AppFeed, HTMLStencilElement {}
  var HTMLAppFeedElement: {
    prototype: HTMLAppFeedElement;
    new (): HTMLAppFeedElement;
  };

  interface HTMLAppFeedCardElement extends Components.AppFeedCard, HTMLStencilElement {}
  var HTMLAppFeedCardElement: {
    prototype: HTMLAppFeedCardElement;
    new (): HTMLAppFeedCardElement;
  };

  interface HTMLAppFeedCardTagsElement extends Components.AppFeedCardTags, HTMLStencilElement {}
  var HTMLAppFeedCardTagsElement: {
    prototype: HTMLAppFeedCardTagsElement;
    new (): HTMLAppFeedCardTagsElement;
  };

  interface HTMLAppFooterElement extends Components.AppFooter, HTMLStencilElement {}
  var HTMLAppFooterElement: {
    prototype: HTMLAppFooterElement;
    new (): HTMLAppFooterElement;
  };

  interface HTMLAppGifElement extends Components.AppGif, HTMLStencilElement {}
  var HTMLAppGifElement: {
    prototype: HTMLAppGifElement;
    new (): HTMLAppGifElement;
  };

  interface HTMLAppHelpElement extends Components.AppHelp, HTMLStencilElement {}
  var HTMLAppHelpElement: {
    prototype: HTMLAppHelpElement;
    new (): HTMLAppHelpElement;
  };

  interface HTMLAppHomeElement extends Components.AppHome, HTMLStencilElement {}
  var HTMLAppHomeElement: {
    prototype: HTMLAppHomeElement;
    new (): HTMLAppHomeElement;
  };

  interface HTMLAppImageElement extends Components.AppImage, HTMLStencilElement {}
  var HTMLAppImageElement: {
    prototype: HTMLAppImageElement;
    new (): HTMLAppImageElement;
  };

  interface HTMLAppImageColumnsElement extends Components.AppImageColumns, HTMLStencilElement {}
  var HTMLAppImageColumnsElement: {
    prototype: HTMLAppImageColumnsElement;
    new (): HTMLAppImageColumnsElement;
  };

  interface HTMLAppLogoElement extends Components.AppLogo, HTMLStencilElement {}
  var HTMLAppLogoElement: {
    prototype: HTMLAppLogoElement;
    new (): HTMLAppLogoElement;
  };

  interface HTMLAppMenuElement extends Components.AppMenu, HTMLStencilElement {}
  var HTMLAppMenuElement: {
    prototype: HTMLAppMenuElement;
    new (): HTMLAppMenuElement;
  };

  interface HTMLAppNavigationElement extends Components.AppNavigation, HTMLStencilElement {}
  var HTMLAppNavigationElement: {
    prototype: HTMLAppNavigationElement;
    new (): HTMLAppNavigationElement;
  };

  interface HTMLAppNavigationActionsElement extends Components.AppNavigationActions, HTMLStencilElement {}
  var HTMLAppNavigationActionsElement: {
    prototype: HTMLAppNavigationActionsElement;
    new (): HTMLAppNavigationActionsElement;
  };

  interface HTMLAppOpensourceElement extends Components.AppOpensource, HTMLStencilElement {}
  var HTMLAppOpensourceElement: {
    prototype: HTMLAppOpensourceElement;
    new (): HTMLAppOpensourceElement;
  };

  interface HTMLAppPhotoElement extends Components.AppPhoto, HTMLStencilElement {}
  var HTMLAppPhotoElement: {
    prototype: HTMLAppPhotoElement;
    new (): HTMLAppPhotoElement;
  };

  interface HTMLAppPopularElement extends Components.AppPopular, HTMLStencilElement {}
  var HTMLAppPopularElement: {
    prototype: HTMLAppPopularElement;
    new (): HTMLAppPopularElement;
  };

  interface HTMLAppPrivacyElement extends Components.AppPrivacy, HTMLStencilElement {}
  var HTMLAppPrivacyElement: {
    prototype: HTMLAppPrivacyElement;
    new (): HTMLAppPrivacyElement;
  };

  interface HTMLAppPublishElement extends Components.AppPublish, HTMLStencilElement {}
  var HTMLAppPublishElement: {
    prototype: HTMLAppPublishElement;
    new (): HTMLAppPublishElement;
  };

  interface HTMLAppRemoteElement extends Components.AppRemote, HTMLStencilElement {}
  var HTMLAppRemoteElement: {
    prototype: HTMLAppRemoteElement;
    new (): HTMLAppRemoteElement;
  };

  interface HTMLAppRootElement extends Components.AppRoot, HTMLStencilElement {}
  var HTMLAppRootElement: {
    prototype: HTMLAppRootElement;
    new (): HTMLAppRootElement;
  };

  interface HTMLAppServicesElement extends Components.AppServices, HTMLStencilElement {}
  var HTMLAppServicesElement: {
    prototype: HTMLAppServicesElement;
    new (): HTMLAppServicesElement;
  };

  interface HTMLAppSettingsElement extends Components.AppSettings, HTMLStencilElement {}
  var HTMLAppSettingsElement: {
    prototype: HTMLAppSettingsElement;
    new (): HTMLAppSettingsElement;
  };

  interface HTMLAppSigninElement extends Components.AppSignin, HTMLStencilElement {}
  var HTMLAppSigninElement: {
    prototype: HTMLAppSigninElement;
    new (): HTMLAppSigninElement;
  };

  interface HTMLAppSlideNavigateElement extends Components.AppSlideNavigate, HTMLStencilElement {}
  var HTMLAppSlideNavigateElement: {
    prototype: HTMLAppSlideNavigateElement;
    new (): HTMLAppSlideNavigateElement;
  };

  interface HTMLAppSlideTypeElement extends Components.AppSlideType, HTMLStencilElement {}
  var HTMLAppSlideTypeElement: {
    prototype: HTMLAppSlideTypeElement;
    new (): HTMLAppSlideTypeElement;
  };

  interface HTMLAppSlotTypeElement extends Components.AppSlotType, HTMLStencilElement {}
  var HTMLAppSlotTypeElement: {
    prototype: HTMLAppSlotTypeElement;
    new (): HTMLAppSlotTypeElement;
  };

  interface HTMLAppTeamElement extends Components.AppTeam, HTMLStencilElement {}
  var HTMLAppTeamElement: {
    prototype: HTMLAppTeamElement;
    new (): HTMLAppTeamElement;
  };

  interface HTMLAppTermsElement extends Components.AppTerms, HTMLStencilElement {}
  var HTMLAppTermsElement: {
    prototype: HTMLAppTermsElement;
    new (): HTMLAppTermsElement;
  };

  interface HTMLAppUserDeleteElement extends Components.AppUserDelete, HTMLStencilElement {}
  var HTMLAppUserDeleteElement: {
    prototype: HTMLAppUserDeleteElement;
    new (): HTMLAppUserDeleteElement;
  };

  interface HTMLAppUserInfoElement extends Components.AppUserInfo, HTMLStencilElement {}
  var HTMLAppUserInfoElement: {
    prototype: HTMLAppUserInfoElement;
    new (): HTMLAppUserInfoElement;
  };

  interface HTMLAppUserMenuElement extends Components.AppUserMenu, HTMLStencilElement {}
  var HTMLAppUserMenuElement: {
    prototype: HTMLAppUserMenuElement;
    new (): HTMLAppUserMenuElement;
  };
  interface HTMLElementTagNameMap {
    'app-about': HTMLAppAboutElement;
    'app-add-slide-action': HTMLAppAddSlideActionElement;
    'app-avatar': HTMLAppAvatarElement;
    'app-code': HTMLAppCodeElement;
    'app-contact': HTMLAppContactElement;
    'app-deck-or-slide': HTMLAppDeckOrSlideElement;
    'app-demo': HTMLAppDemoElement;
    'app-developer': HTMLAppDeveloperElement;
    'app-editor': HTMLAppEditorElement;
    'app-editor-actions': HTMLAppEditorActionsElement;
    'app-editor-toolbar': HTMLAppEditorToolbarElement;
    'app-feed': HTMLAppFeedElement;
    'app-feed-card': HTMLAppFeedCardElement;
    'app-feed-card-tags': HTMLAppFeedCardTagsElement;
    'app-footer': HTMLAppFooterElement;
    'app-gif': HTMLAppGifElement;
    'app-help': HTMLAppHelpElement;
    'app-home': HTMLAppHomeElement;
    'app-image': HTMLAppImageElement;
    'app-image-columns': HTMLAppImageColumnsElement;
    'app-logo': HTMLAppLogoElement;
    'app-menu': HTMLAppMenuElement;
    'app-navigation': HTMLAppNavigationElement;
    'app-navigation-actions': HTMLAppNavigationActionsElement;
    'app-opensource': HTMLAppOpensourceElement;
    'app-photo': HTMLAppPhotoElement;
    'app-popular': HTMLAppPopularElement;
    'app-privacy': HTMLAppPrivacyElement;
    'app-publish': HTMLAppPublishElement;
    'app-remote': HTMLAppRemoteElement;
    'app-root': HTMLAppRootElement;
    'app-services': HTMLAppServicesElement;
    'app-settings': HTMLAppSettingsElement;
    'app-signin': HTMLAppSigninElement;
    'app-slide-navigate': HTMLAppSlideNavigateElement;
    'app-slide-type': HTMLAppSlideTypeElement;
    'app-slot-type': HTMLAppSlotTypeElement;
    'app-team': HTMLAppTeamElement;
    'app-terms': HTMLAppTermsElement;
    'app-user-delete': HTMLAppUserDeleteElement;
    'app-user-info': HTMLAppUserInfoElement;
    'app-user-menu': HTMLAppUserMenuElement;
  }
}

declare namespace LocalJSX {
  interface AppAbout extends JSXBase.HTMLAttributes<HTMLAppAboutElement> {}
  interface AppAddSlideAction extends JSXBase.HTMLAttributes<HTMLAppAddSlideActionElement> {
    'onActionOpenSlideAdd'?: (event: CustomEvent<UIEvent>) => void;
  }
  interface AppAvatar extends JSXBase.HTMLAttributes<HTMLAppAvatarElement> {
    'src'?: string;
  }
  interface AppCode extends JSXBase.HTMLAttributes<HTMLAppCodeElement> {
    'codeDidChange'?: EventEmitter<HTMLElement>;
    'selectedElement'?: HTMLElement;
  }
  interface AppContact extends JSXBase.HTMLAttributes<HTMLAppContactElement> {}
  interface AppDeckOrSlide extends JSXBase.HTMLAttributes<HTMLAppDeckOrSlideElement> {}
  interface AppDemo extends JSXBase.HTMLAttributes<HTMLAppDemoElement> {}
  interface AppDeveloper extends JSXBase.HTMLAttributes<HTMLAppDeveloperElement> {}
  interface AppEditor extends JSXBase.HTMLAttributes<HTMLAppEditorElement> {
    'deckId'?: string;
  }
  interface AppEditorActions extends JSXBase.HTMLAttributes<HTMLAppEditorActionsElement> {}
  interface AppEditorToolbar extends JSXBase.HTMLAttributes<HTMLAppEditorToolbarElement> {
    'onBlockSlide'?: (event: CustomEvent<boolean>) => void;
    'onCodeDidChange'?: (event: CustomEvent<HTMLElement>) => void;
    'onDeckDidChange'?: (event: CustomEvent<HTMLElement>) => void;
    'onSlideDelete'?: (event: CustomEvent<HTMLElement>) => void;
    'onSlideDidChange'?: (event: CustomEvent<HTMLElement>) => void;
  }
  interface AppFeed extends JSXBase.HTMLAttributes<HTMLAppFeedElement> {}
  interface AppFeedCard extends JSXBase.HTMLAttributes<HTMLAppFeedCardElement> {
    'author'?: string;
    'caption'?: string;
    'compact'?: boolean;
    'description'?: string;
    'miniature'?: boolean;
    'publication'?: Date;
  }
  interface AppFeedCardTags extends JSXBase.HTMLAttributes<HTMLAppFeedCardTagsElement> {
    'editable'?: boolean;
    'onRemoveTag'?: (event: CustomEvent<string>) => void;
    'tags'?: string[];
  }
  interface AppFooter extends JSXBase.HTMLAttributes<HTMLAppFooterElement> {}
  interface AppGif extends JSXBase.HTMLAttributes<HTMLAppGifElement> {}
  interface AppHelp extends JSXBase.HTMLAttributes<HTMLAppHelpElement> {}
  interface AppHome extends JSXBase.HTMLAttributes<HTMLAppHomeElement> {}
  interface AppImage extends JSXBase.HTMLAttributes<HTMLAppImageElement> {
    'deckOrSlide'?: boolean;
  }
  interface AppImageColumns extends JSXBase.HTMLAttributes<HTMLAppImageColumnsElement> {
    'imagesEven'?: (UnsplashPhoto | TenorGif)[];
    'imagesOdd'?: (UnsplashPhoto | TenorGif)[];
    'onSelectImage'?: (event: CustomEvent<UnsplashPhoto | TenorGif>) => void;
  }
  interface AppLogo extends JSXBase.HTMLAttributes<HTMLAppLogoElement> {}
  interface AppMenu extends JSXBase.HTMLAttributes<HTMLAppMenuElement> {}
  interface AppNavigation extends JSXBase.HTMLAttributes<HTMLAppNavigationElement> {
    'logo'?: boolean;
    'menuToggle'?: boolean;
    'presentation'?: boolean;
    'publish'?: boolean;
    'user'?: boolean;
  }
  interface AppNavigationActions extends JSXBase.HTMLAttributes<HTMLAppNavigationActionsElement> {
    'onActionPublish'?: (event: CustomEvent<void>) => void;
    'presentation'?: boolean;
    'publish'?: boolean;
    'signIn'?: boolean;
  }
  interface AppOpensource extends JSXBase.HTMLAttributes<HTMLAppOpensourceElement> {}
  interface AppPhoto extends JSXBase.HTMLAttributes<HTMLAppPhotoElement> {}
  interface AppPopular extends JSXBase.HTMLAttributes<HTMLAppPopularElement> {}
  interface AppPrivacy extends JSXBase.HTMLAttributes<HTMLAppPrivacyElement> {}
  interface AppPublish extends JSXBase.HTMLAttributes<HTMLAppPublishElement> {
    'description'?: string;
  }
  interface AppRemote extends JSXBase.HTMLAttributes<HTMLAppRemoteElement> {}
  interface AppRoot extends JSXBase.HTMLAttributes<HTMLAppRootElement> {}
  interface AppServices extends JSXBase.HTMLAttributes<HTMLAppServicesElement> {}
  interface AppSettings extends JSXBase.HTMLAttributes<HTMLAppSettingsElement> {}
  interface AppSignin extends JSXBase.HTMLAttributes<HTMLAppSigninElement> {
    'redirect'?: string;
    'redirectId'?: string;
  }
  interface AppSlideNavigate extends JSXBase.HTMLAttributes<HTMLAppSlideNavigateElement> {
    'slides'?: string[];
  }
  interface AppSlideType extends JSXBase.HTMLAttributes<HTMLAppSlideTypeElement> {}
  interface AppSlotType extends JSXBase.HTMLAttributes<HTMLAppSlotTypeElement> {
    'selectedElement'?: HTMLElement;
  }
  interface AppTeam extends JSXBase.HTMLAttributes<HTMLAppTeamElement> {}
  interface AppTerms extends JSXBase.HTMLAttributes<HTMLAppTermsElement> {}
  interface AppUserDelete extends JSXBase.HTMLAttributes<HTMLAppUserDeleteElement> {
    'username'?: string;
  }
  interface AppUserInfo extends JSXBase.HTMLAttributes<HTMLAppUserInfoElement> {
    'avatarColSize'?: number;
  }
  interface AppUserMenu extends JSXBase.HTMLAttributes<HTMLAppUserMenuElement> {}

  interface IntrinsicElements {
    'app-about': AppAbout;
    'app-add-slide-action': AppAddSlideAction;
    'app-avatar': AppAvatar;
    'app-code': AppCode;
    'app-contact': AppContact;
    'app-deck-or-slide': AppDeckOrSlide;
    'app-demo': AppDemo;
    'app-developer': AppDeveloper;
    'app-editor': AppEditor;
    'app-editor-actions': AppEditorActions;
    'app-editor-toolbar': AppEditorToolbar;
    'app-feed': AppFeed;
    'app-feed-card': AppFeedCard;
    'app-feed-card-tags': AppFeedCardTags;
    'app-footer': AppFooter;
    'app-gif': AppGif;
    'app-help': AppHelp;
    'app-home': AppHome;
    'app-image': AppImage;
    'app-image-columns': AppImageColumns;
    'app-logo': AppLogo;
    'app-menu': AppMenu;
    'app-navigation': AppNavigation;
    'app-navigation-actions': AppNavigationActions;
    'app-opensource': AppOpensource;
    'app-photo': AppPhoto;
    'app-popular': AppPopular;
    'app-privacy': AppPrivacy;
    'app-publish': AppPublish;
    'app-remote': AppRemote;
    'app-root': AppRoot;
    'app-services': AppServices;
    'app-settings': AppSettings;
    'app-signin': AppSignin;
    'app-slide-navigate': AppSlideNavigate;
    'app-slide-type': AppSlideType;
    'app-slot-type': AppSlotType;
    'app-team': AppTeam;
    'app-terms': AppTerms;
    'app-user-delete': AppUserDelete;
    'app-user-info': AppUserInfo;
    'app-user-menu': AppUserMenu;
  }
}

export { LocalJSX as JSX };


declare module "@stencil/core" {
  export namespace JSX {
    interface IntrinsicElements extends LocalJSX.IntrinsicElements {}
  }
}


