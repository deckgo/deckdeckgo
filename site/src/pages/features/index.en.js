import * as React from 'react';

import Layout from '../../components/core/layout/en';

import {Footer} from '../../components/core/footer/footer';
import {Features} from '../../components/enterprise/features/features';
import {Contact} from '../../components/enterprise/contact/contact';
import {FeaturesHero} from '../../components/features/hero/hero';

const FeaturesPages = (props) => {
  return (
    <Layout location={props.location} sticky={false} navTheme={'features'}>
      <FeaturesHero />

      <Features />

      <Contact />

      <Footer lang="en" action={false} />
    </Layout>
  );
};

export default FeaturesPages;
