
import { randomName as randomName } from '../../variables';

const customer = {
  "name": "Verizon New " + new Date(),
  "street1": "700 Hidden Ridge",
  "street2": "Block W",
  "city": "Irving",
  "state": "Texas",
  "postal_code": "75888",
  "country": "United States",
  "contact_name": "Smith Jones",
  "contact_phone": "(987)654-3219",
  "contact_email": "verizon@one.verizon.com",
  "save_class": "btn-success"
};

const partner = {
  "name": "UI Test Automation " + new Date(),
  "type": "partner",
  "street1": "700 Hidden Ridge",
  "street2": "Block W",
  "city": "Irving",
  "state": "Texas",
  "postal_code": "75888",
  "country": "United States",
  "contact": "(987)6543219",
  "contact_name": "Super Man",
  "save_class": "btn-success"
};

export { customer as customer, partner };
