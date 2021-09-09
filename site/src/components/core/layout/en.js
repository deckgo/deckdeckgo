import React from 'react';
import Layout from './layout';

import index from '../../../assets/i18n/index.json';
import enterprise from '../../../assets/i18n/enterprise.json';
import features from '../../../assets/i18n/features.json';
import pricing from '../../../assets/i18n/pricing.json';
import discover from '../../../assets/i18n/discover.json';
import footer from '../../../assets/i18n/footer.json';
import nav from '../../../assets/i18n/nav.json';
import common from '../../../assets/i18n/common.json';

import '@formatjs/intl-pluralrules/locale-data/en';

const LayoutEn = (props) => (
  <Layout
    {...props}
    messages={{
      ...index,
      ...footer,
      ...nav,
      ...common,
      ...enterprise,
      ...features,
      ...pricing,
      ...discover
    }}
  />
);

export default LayoutEn;
