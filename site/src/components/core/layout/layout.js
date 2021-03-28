import React from 'react';
import {useStaticQuery, graphql} from 'gatsby';

import {IntlProvider} from 'react-intl';
import '@formatjs/intl-pluralrules/polyfill';

import {getCurrentLangKey} from 'ptz-i18n';

import '../../../themes/fonts.scss';
import '../../../themes/theme.scss';
import '../../../themes/main.scss';
import '../../../themes/input.scss';
import '../../../themes/variables.scss';

import SEO from '../seo/seo';
import Navigation from '../navigation/navigation';

import {sticky as stickyStyles} from './layout.module.scss';

import {defineCustomElements as deckDeckGoCoreElement} from '@deckdeckgo/core/dist/loader';

import {defineCustomElements as deckDeckGoSlideTitleElement} from '@deckdeckgo/slide-title/dist/loader';
import {defineCustomElements as deckDeckGoSlideSplitElement} from '@deckdeckgo/slide-split/dist/loader';
import {defineCustomElements as deckDeckGoSlideAuthorElement} from '@deckdeckgo/slide-author/dist/loader';
import {defineCustomElements as deckDeckGoSlidePollElement} from '@deckdeckgo/slide-poll/dist/loader';

import {defineCustomElements as deckDeckGoHighlightCode} from '@deckdeckgo/highlight-code/dist/loader';
import {defineCustomElements as deckDeckGoYoutube} from '@deckdeckgo/youtube/dist/loader';
import {defineCustomElements as deckDeckGoCharts} from '@deckdeckgo/charts/dist/loader';
import {defineCustomElements as deckDeckGoQRCode} from '@deckdeckgo/qrcode/dist/loader';

deckDeckGoCoreElement();
deckDeckGoSlideTitleElement();
deckDeckGoSlideSplitElement();
deckDeckGoSlideAuthorElement();
deckDeckGoSlidePollElement();
deckDeckGoHighlightCode();
deckDeckGoYoutube();
deckDeckGoCharts();
deckDeckGoQRCode();

const Layout = ({children, location, messages, sticky = true, dark}) => {
  const data = useStaticQuery(graphql`
    query SiteTitleQuery {
      site {
        siteMetadata {
          title
          languages {
            defaultLangKey
            langs
          }
        }
      }
    }
  `);

  const {langs, defaultLangKey} = data.site.siteMetadata.languages;
  const langKey = getCurrentLangKey(langs, defaultLangKey, location.pathname);

  return (
    <IntlProvider locale={langKey} messages={messages}>
      <SEO lang={langKey} />

      <Navigation lang={langKey} dark={dark} />

      <div className={sticky ? stickyStyles : undefined}>{children}</div>
    </IntlProvider>
  );
};

export default Layout;

// IE9: https://stackoverflow.com/questions/5472938/does-ie9-support-console-log-and-is-it-a-real-function#answer-5473193
const log = Function.prototype.bind.call(console.log, console);
log.apply(console, ['%cDeckDeckGo', 'color: #3880ff;font-size:2rem;font-weight: 300;']);
log.apply(console, ['%cHey 👋! We are open source 😃.', 'color: #3dc2ff;font-size:1rem;font-weight: 300;']);
log.apply(console, ['%cCome say hi and contribute to our project on Github.', 'color: #3dc2ff;font-size:1rem;font-weight: 300;']);
log.apply(console, ['%chttps://github.com/deckgo/deckdeckgo', 'font-size:1rem;font-weight: 300;']);
