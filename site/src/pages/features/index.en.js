import * as React from 'react';

import Layout from '../../components/core/layout/en';

import {Footer} from '../../components/core/footer/footer';
import {FeaturesHero} from '../../components/features/hero/hero';
import {ListFeatures} from '../../components/features/features/features';

const FeaturesPages = (props) => {
  return (
    <Layout location={props.location} sticky={false} navTheme={'features'}>
      <FeaturesHero />

      <ListFeatures lang="en" />

      <Footer lang="en" action={false} />
    </Layout>
  );
};

export default FeaturesPages;
