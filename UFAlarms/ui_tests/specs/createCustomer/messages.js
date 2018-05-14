import { customer, partner } from './mockData';

const messages = {
  "customer_sucess": 'Customer "' + customer.name + '" added.',
  "customer_warning": 'Customer "' + customer.name + '" already exists.',
  "partner_sucess": 'Customer "' + partner.name + '" added.',
  "partner_warning": 'Customer "' + partner.name + '" already exists.',

};

export { messages as msgs };
