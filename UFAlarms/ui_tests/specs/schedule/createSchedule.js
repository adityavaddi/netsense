'use strict';
import * as input from '../../variables';
import schedulePage from './schedulePage'

describe('Creating a schedule . . .', () => {

    it('Should click on add Schedule button', () => {
        /* This sleep Time is to Select the customer and Site and Go to Schedule Panel Manually 
        until the generic test cases do this set-up */
        browser.sleep(10000)
        schedulePage.clickOnAddSchedule()
    })

    it('Should click on Settings Panel', () => {
        schedulePage.openSettingsPanel()
    })

    it('Should enter name: ' + input.schedule.name + ' and description: ' + input.schedule.desc, () => {

        schedulePage.addScheduleName(input.schedule.name)
        schedulePage.addScheduleDesc(input.schedule.desc)
    })

    it('High Time should be 18:00:00', () => {
        $('#schedhitime').getAttribute('value').then(function(value) {
            expect(value).to.equal("18:00:00")

        })
    })

    it('High Level should be 60', () => {
        $('#schedhilevel').getAttribute('value').then(function(value) {
            expect(value).to.equal("60")
        })
    })

    it('Low Time should be 06:00:00', () => {
        $('#schedlotime').getAttribute('value').then(function(value) {
            expect(value).to.equal("06:00:00")

        })
    })

    it('Low Level should be 0', () => {
        $('#schedlolevel').getAttribute('value').then(function(value) {
            expect(value).to.equal("0")
        })
    })

    it('Should click on Timeline 1', () => {
        schedulePage.openTimeline()
    })

    it('Drag and Drop Sun and time Icons onto the Timeline', () => {
        //Place holding the Test Case untill drag and drop is explored
        expect(true).to.equal(true)
    })

    it('Should save schedule', () => {
        schedulePage.submit()
        browser.sleep(5000)
    })
})