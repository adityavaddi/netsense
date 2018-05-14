'use strict';

import React from 'react';
import { shallow, mount, render } from 'enzyme';
import { chai, assert, expect } from 'chai';
import { customer, error } from '../__mocks__/customer';
import Customerform from '../../src/jsx/app/components/customers/customerform';

// Customerform test suite
describe("Customer form Check", function () {
  it("contains spec with an expectation", function () {
    const wrapper = shallow(<Customerform customer={customer} error={error} />)
    expect(wrapper.find('.form-group')).to.have.length(11);
  });
});
