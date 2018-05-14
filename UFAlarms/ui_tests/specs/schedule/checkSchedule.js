import * as input from '../../variables';
import schedulePage from './schedulePage'

'use strict';
describe('Checking schedule just created . . .', () => {

    it('Should search for a ' + input.schedule.name + ' schedule ', () => {
       schedulePage.selectSchedule(input.schedule.name)
    })

    it('Should pull up settings panel', () => {
        schedulePage.openSettingsPanel()
    })

    it('Schedule Name should be: ' + input.schedule.name, () => {
        $('#schedname').getAttribute('value').then(function(value) {
            expect(value).to.equal(input.schedule.name)
        })
    })

    it('Schedule Description should be: ' + input.schedule.desc, () => {
        $('#scheddescription').getAttribute('value').then(function(value) {
            expect(value).to.equal(input.schedule.desc)
        })
    })

    it('Check timeline for schedule to be 70% at 06:00:32', () => {
        //Place holding the Test Case untill drag and drop is explored
        expect(true).to.equal(true)
    })

    it('Should click on Timeline 1', () => {
        schedulePage.openTimeline()
        browser.sleep(10000)
    })

})