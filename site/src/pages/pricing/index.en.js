import * as React from 'react';

import Layout from '../../components/core/layout/en';

import {Footer} from '../../components/core/footer/footer';
import {PricingHero} from '../../components/pricing/hero/hero';
import {Plan} from '../../components/pricing/plan/plan';

const PricingPages = (props) => {
  return (
    <Layout location={props.location} sticky={false} navTheme={'pricing'}>
      <PricingHero></PricingHero>

      <Plan lang="en"></Plan>

      <Footer lang="en" action={false} />
    </Layout>
  );
};

export default PricingPages;
