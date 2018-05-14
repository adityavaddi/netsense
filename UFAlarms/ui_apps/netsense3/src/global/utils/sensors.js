 var sensors = {
    getSensorsList: function() {

/* The sensors list is an array of descriptors used by the UI to 
      - generate the Diagnostics and Reports accordions in the Nodes page
      - generate the Sensors dropdowns in the Reporting page

    Each element is an object with seven fields:
         id :  the hardware-defined sensor id (must be unique)
         type:  the type of node to which this sensor applies (Lighting, Video or Kiosk)
         model:  the specific models that support this sensor
         category:  groups the sensors for the accordions (Diagnostic, Network, Ambient, Energy) in the Nodes page
         order: specifies the order in which the sensor appears in those accordions
         name: the name of the sensor as displayed to the user
         units: the units for the sensor value (appended to value in UI)

    This is the single source of truth for sensors in the UI

*/
         return [
        {
            id: 'acc0',
            type: ['Video'],
            model: ['merlin'],
            category: 'Diagnostic',
            order: 1,
            name: 'Camera 0 Accelerometer',
            units: ' (v3fn)'
        },
        {
            id: 'acc1',
            type: ['Video'],
            model: ['merlin'],
            category: 'Diagnostic',
            order: 2,
            name: 'Camera 1 Accelerometer',
            units: ' (v3fn)'
        },
        {
            id: 'acm0',
            type: ['Video', 'Lighting'],
            model: ['merlin', 'cnext'],
            category: 'Diagnostic',
            order: 3,
            name: 'Mainboard Accelerometer',
            units: ' (v3fn)'
        },
        {
            id: 'cam_frame',
            type: ['Video'],
            model: ['merlin'],
            category: 'Diagnostic',
            order: 5.5,
            name: 'Frames Captured by Camera',
            units: ' '
        },
        {
            id: 'cell_lvl',
            type: ['Lighting'],
            model: ['cnext'],
            category: 'Network',
            order: 12,
            name: 'Cell Connection Signal Level',
            units: ' '
        },        
        {
            id: 'cnt_eeplogd',
            type: ['Video', 'Lighting'],
            model: ['falcon-q','merlin','cnext'],
            category: 'Diagnostic',
            order: 12,
            name: 'Restarts: eeplogd',
            units: ' '
        },        
        {
            id: 'bR',
            type: ['Lighting'],
            model: ['cnext'],
            category: 'Diagnostic',
            order: 13,
            name: 'Boot reason',
            units: ' '
        },        
        {
            id: 'cnt_genetec',
            type: ['Video'],
            model: ['falcon-q','merlin'],
            category: 'Diagnostic',
            order: 13,
            name: 'Restarts: genetec',
            units: ' '
        },        
        {
            id: 'cnt_mediaserver',
            type: ['Video'],
            model: ['falcon-q','merlin'],
            category: 'Diagnostic',
            order: 14,
            name: 'Restarts: mediaserver',
            units: ' '
        },        
        {
            id: 'cnt_mqtt',
            type: ['Video', 'Lighting'],
            model: ['falcon-q','merlin','cnext'],
            category: 'Diagnostic',
            order: 15,
            name: 'Restarts: mqtt',
            units: ' '
        },        
        {
            id: 'cnt_network_manager',
            type: ['Video','Lighting'],
            model: ['falcon-q','merlin','cnext'],
            category: 'Diagnostic',
            order: 16,
            name: 'Restarts: NetworkManager',
            units: ' '
        },        
        {
            id: 'cnt_nirung',
            type: ['Video','Lighting'],
            model: ['falcon-q','merlin','cnext'],
            category: 'Diagnostic',
            order: 17,
            name: 'Restarts: nirung',
            units: ' '
        },        
         {
            id: 'cnt_nmm',
            type: ['Video','Lighting'],
            model: ['falcon-q','merlin','cnext'],
            category: 'Diagnostic',
            order: 18,
            name: 'Restarts: NMM',
            units: ' '
        },        
        {
            id: 'cnt_uboot',
            type: ['Video','Lighting'],
            model: ['falcon-q','merlin','cnext'],
            category: 'Diagnostic',
            order: 19,
            name: 'Restarts: UBoot',
            units: ' '
        },        
       {
            id: 'aip',
            type: ['Lighting'],
            model: ['unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Diagnostic',
            order: 12,
            name: 'Auxiliary current spike',
            units: ' (A)'
        },    
        {
            id: 'T',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Diagnostic',
            order: 13,
            name: 'Internal Temperature',
            units: ' (˚C)'
        }, 
        {
            id: 'WDT',
            type: ['Lighting'],
            model: [ 'cnext' ],
            category: 'Diagnostic',
            order: 13.1,
            name: 'Watchdog Reset Count',
            units: ' '
        }, 
        {
            id: 'jtx',
            type: ['Lighting'],
            model: ['unode-v3', 'unode-v4', 'unode-v5', 'unode-v6' ],
            category: 'Diagnostic',
            order: 14,
            name: 'Jolt X',
            units: ' '
        }, 
        {
            id: 'jty',
            type: ['Lighting'],
            model: ['unode-v3', 'unode-v4', 'unode-v5', 'unode-v6' ],
            category: 'Diagnostic',
            order: 15,
            name: 'Jolt Y',
            units: ' '
        }, 
        {
            id: 'jtz',
            type: ['Lighting'],
            model: ['unode-v3', 'unode-v4', 'unode-v5', 'unode-v6' ],
            category: 'Diagnostic',
            order: 16,
            name: 'Jolt Z',
            units: ' '
        }, 
        {
            id: 'jtm',
            type: ['Lighting'],
            model: ['unode-v3', 'unode-v4', 'unode-v5', 'unode-v6' ],
            category: 'Diagnostic',
            order: 17,
            name: 'Jolt M',
            units: ' '
        }, 
        {
            id: 'rlym',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Diagnostic',
            order: 18,
            name: 'Main Relay Cycle',
            units: ' '
        },
        {
            id: 'mt',
            type: ['Lighting'],
            model: ['unode-v3', 'unode-v4', 'unode-v5', 'unode-v6' ],
            category: 'Diagnostic',
            order: 19,
            name: 'MCU Temperature',
            units: ' (˚C)'
        }, 
        {
            id: 'RF',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6' ],
            category: 'Diagnostic',
            order: 20,
            name: 'Received Signal Strength & Network Connection Noise',
            units: ' (dBm)'
        },
        {
            id: 'podme',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Diagnostic',
            order: 21,
            name: 'Response Error Count',
            units: ' '
        }, 
        {
            id: 'podm',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Diagnostic',
            order: 22,
            name: 'Total Message And Error Count',
            units: ' '
        }, 
        {
            id: 'vp',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Diagnostic',
            order: 23,
            name: 'Voltage Spike',
            units: ' (V)'
        }, 
        {
            id: 'pdc',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6' ],
            category: 'Diagnostic',
            order: 24,
            name: 'Zmotion PIR DC Value',
            units: ' '
        },
        {
            id: 'ppr',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Diagnostic',
            order: 25,
            name: 'Zmotion PIR Process Rate',
            units: ' '
        },
        {
            id: 'pnd',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6' ],
            category: 'Diagnostic',
            order: 26,
            name: 'Zmotion EM Noise Detected',
            units: ' '
        },
        {
            id: 'pdt',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6' ],
            category: 'Diagnostic',
            order: 27,
            name: 'Zmotion EM Transient Detected',
            units: ' '
        },
        {
            id: 'df_',
            type: ['Video', 'Kiosk', 'Lighting'],
            model: ['falcon-q', 'merlin', 'vdkmaster', 'cnext'],
            category: 'Network',
            order: 1,
            name: 'Free Disk Space on /(rootfs)',
            units: ' (B)'
        },
        {   
            id: 'df_data',
            type: ['Video', 'Kiosk', 'Lighting'],
            model: ['falcon-q', 'merlin', 'vdkmaster', 'cnext'],
            category: 'Network',
            order: 2,
            name: 'Free Disk Space on /data',
            units: ' (B)'
        },
        {
            id: 'df_disk',
            type: ['Video', 'Lighting'],
            model: ['falcon-q', 'merlin', 'cnext'],
            category: 'Network',
            order: 3,
            name: 'Free Disk Space on /disk',
            units: ' (B)'
        },
        {
            id: 'memt',
            type: ['Video', 'Kiosk', 'Lighting'],
            model: ['falcon-q', 'merlin', 'vdkmaster', 'cnext'],
            category: 'Network',
            order: 4,
            name: 'Total System Memory',
            units: ' (B)'
        },
        {
            id: 'memf',
            type: ['Video', 'Kiosk', 'Lighting'],
            model: ['falcon-q', 'merlin', 'vdkmaster', 'cnext'],
            category: 'Network',
            order: 5,
            name: 'Total Free Memory',
            units: ' (B)'
        },
        {
            id: 'nbr_eth0',
            type: ['Video', 'Kiosk', 'Lighting'],
            model: ['falcon-q','merlin','vdkmaster', 'cnext'],
            category: 'Network',
            order: 6,
            name: 'Bytes Received on eth0',
            units: ' (mLAV)'
        },
        {
            id: 'nbr_wlan0',
            type: ['Video', 'Lighting'],
            model: ['falcon-q','merlin', 'cnext'],
            category: 'Network',
            order: 7,
            name: 'Bytes Received on wlan0',
            units: ' (mLAV)'
        },
        {
            id: 'nbt_eth0',
            type: ['Video', 'Kiosk', 'Lighting'],
            model: ['falcon-q','merlin','vdkmaster','cnext'],
            category: 'Network',
            order: 8,
            name: 'Bytes Transmitted on eth0',
            units: ' (mLAV)'
        },
        {
            id: 'nbt_wlan0',
            type: ['Video','Lighting'],
            model: ['falcon-q','merlin','cnext'],
            category: 'Network',
            order: 9,
            name: 'Bytes Transmitted on wlan0',
            units: ' (mLAV)'
        },
        {
            id: 'rf',
            type: ['Video', 'Lighting'],
            model: ['falcon-q', 'merlin', 'cnext'],
            category: 'Network',
            order: 11,
            name: 'Network Connection Signal',
            units: ' (RFsn)'
        },
        {
            id: 'uptime',
            type: ['Video', 'Kiosk', 'Lighting'],
            model: ['falcon-q', 'merlin', 'vdkmaster', 'cnext'],
            category: 'Network',
            order: 12,
            name: 'System Uptime',
            units: ' (sec)'
        },
        {
            id: 'disk_life',
            type: ['Video', 'Lighting'],
            model: ['falcon-q', 'merlin', 'cnext'],
            category: 'Diagnostic',
            order: 20,
            name: 'Disk Life',
            units: ' (%)'
        },
        {
            id: 'disk_usage',
            type: ['Video', 'Lighting'],
            model: ['falcon-q', 'merlin', 'cnext'],
            category: 'Diagnostic',
            order: 21,
            name: 'Disk Usage (32MB writes)',
            units: ' '
        },
        {            
            id: 'rlya',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Network',
            order: 13,
            name: 'Aux Relay Cycle',
            units: ' '
        },
        {
            id: 'bc',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Network',
            order: 14,
            name: 'Boot count',
            units: ' '
        }, 
        {
            id: 'c',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6' ],
            category: 'Network',
            order: 15,
            name: 'Connected',
            units: ' '
        },
        {
            id: 'cc',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6' ],
            category: 'Network',
            order: 16,
            name: 'Connected count',
            units: ' '
        },
        {
            id: 'mip',
            type: ['Lighting'],
            model: ['unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Network',
            order: 17,
            name: 'Main current spike',
            units: ' (A)'
        },
        {
            id: 'sn',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6' ],
            category: 'Network',
            order: 18,
            name: 'Network conn noise level',
            units: ' (dBm)'
        },     
        {
            id: 'lav',
            type: ['Video', 'Kiosk', 'Lighting'],
            model: ['falcon-q', 'merlin', 'vdkmaster', 'cnext'],
            category: 'Network',
            order: 19,
            name: 'CPU and IO utilization (last minute)',
            units: ' (mLAV)'
        },     
        {
            id: 'ct0',
            type: ['Video'],
            model: ['falcon-q'],
            category: 'Ambient',
            order: 0.5,
            name: 'Camera 0 Temperature',
            units: ' (C)'
        },
        {
            id: 'ct1',
            type: ['Video'],
            model: ['falcon-q'],
            category: 'Ambient',
            order: 0.6,
            name: 'Camera 1 Temperature',
            units: ' (C)'
        },
         {
            id: 'tcore',
            type: ['Video', 'Kiosk', 'Lighting'],
            model: ['falcon-q', 'merlin', 'vdkmaster', 'cnext'],
            category: 'Ambient',
            order: 1,
            name: 'CPU Core Temperature',
            units: ' (mC)'
        },
        {
            id: 'tgrl',
            type: ['Lighting'],
            model: ['cnext'],
            category: 'Ambient',
            order: 2,
            name: 'Ambient Low Light Trigger',
            units: ' '
        },
        {
            id: 'tsys',
            type: ['Video', 'Lighting'],
            model: ['falcon-q', 'merlin', 'cnext'],
            category: 'Ambient',
            order: 2,
            name: 'System Temperature',
            units: ' (mC)'
        },
        {
            id: 'snd_ambient',
            type: ['Video','Lighting'],
            model: ['falcon-q', 'merlin', 'cnext'],
            category: 'Ambient',
            order: 3,
            name: 'Ambient Sound Level',
            units: ' (uBSPL)'
        },
        {
            id: 'snow_depth',
            type: ['Video'],
            model: ['falcon-q', 'merlin'],
            category: 'Ambient',
            order: 3.5,
            name: 'Snow Depth',
            units: ' (mm)'
        },
        {
            id: 't',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Ambient',
            order: 4,
            name: 'Ambient Temperature',
            units: ' (˚C)'
        }, 
        {
            id: 'ai',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Energy',
            order: 1,
            name: 'Auxiliary current',
            units: ' (A)'
        }, 
        {
            id: 'aw',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Energy',
            order: 2,
            name: 'Auxiliary Energy use',
            units: ' (WH)'
        },
        {
            id: 'aP',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Energy',
            order: 3,
            name: 'Auxiliary Power',
            units: ' (W)'
        },
        {
            id: 'aPF',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Energy',
            order: 4,
            name: 'Auxiliary power factor',
            units: ' '
        },
        {
            id: 'lt',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Ambient',
            order: 5,
            name: 'Driver Level',
            units: ' (%)'
        },
        {
            id: 'lIR',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Ambient',
            order: 6,
            name: 'Infrared Sensor',
            units: ' (lux)'
        }, 
        {
            id: 'lIR-i',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6' ],
            category: 'Ambient',
            order: 7,
            name: 'Internal Infrared Sensor',
            units: ' (lux)'
        },
        {
            id: 'l',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Ambient',
            order: 8,
            name: 'Light Level',
            units: ' (lux)'
        },
        {
            id: 'l-i',
            type: ['Lighting'],
            model: ['unode-v5', 'unode-v6' ],
            category: 'Ambient',
            order: 9,
            name: 'Ambient Light Level',
            units: ' (lux)'
        },
        {
            id: 'mi',
            type: ['Lighting'],
            model: ['unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Energy',
            order: 5,
            name: 'Main Current',
            units: ' (A)'
        }, 
        {
            id: 'i',
            type: ['Lighting'],
            model: ['unode-v2'],
            category: 'Energy',
            order: 5,
            name: 'Main Current',
            units: ' (A)'
        }, 
        {
            id: 'mw',
            type: ['Lighting'],
            model: ['unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Energy',
            order: 6,
            name: 'Main Energy Use',
            units: ' (WH)'
        }, 
        {
            id: 'w',
            type: ['Lighting'],
            model: ['unode-v2'],
            category: 'Energy',
            order: 6,
            name: 'Main Energy Use',
            units: ' (WH)'
        }, 
        {            
            id: 'mP',
            type: ['Lighting'],
            model: ['unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Energy',
            order: 7,
            name: 'Main Power',
            units: ' (W)'
        }, 
        {            
            id: 'P',
            type: ['Lighting'],
            model: ['unode-v2'],
            category: 'Energy',
            order: 7,
            name: 'Main Power',
            units: ' (W)'
        }, 
        {
            id: 'mPF',
            type: ['Lighting'],
            model: ['unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Energy',
            order: 8,
            name: 'Main Power Factor',
            units: ' '
        },
        {
            id: 'PF',
            type: ['Lighting'],
            model: ['unode-v2'],
            category: 'Energy',
            order: 8,
            name: 'Main Power Factor',
            units: ' '
        },
        {
            id: 'p',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Ambient',
            order: 9,
            name: 'Presence Detector',
            units: ' '
        },  
        {   id: 'pc',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Ambient',
            order: 10,
            name: 'Presence count',
            units: ' '
        }, 
        {
            id: 'v',
            type: ['Lighting'],
            model: ['unode-v2', 'unode-v3', 'unode-v4', 'unode-v5', 'unode-v6', 'cnext' ],
            category: 'Energy',
            order: 9,
            name: 'Voltage',
            units: ' (V)'
        },
        {
            id: 'gl_display_tmp',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 2,
            name: 'Glass Left board Display Temperature',
            units: ' (B)'
        },
        {
            id: 'gl_dfree',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 2,
            name: 'Glass Left board free disk space',
            units: ' (B)'
        },
        {
            id: 'gl_dspace',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 2,
            name: 'Glass Left board disk space',
            units: ' (B)'
        },
        {
            id: 'gl_l_avg_1',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 2,
            name: 'Glass Left board load avg (1 min)',
            units: ' (mLAV)'
        },
        {
            id: 'gl_l_avg_15',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 4,
            name: 'Glass Left board load avg (15 min)',
            units: ' (mLAV)'
        },
        {
            id: 'gl_l_avg_5',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 3,
            name: 'Glass Left board load avg (5 min)',
            units: ' (mLav)'
        },
        {
            id: 'gl_tmp_cpu0_0',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 2.1,
            name: 'Glass Left board cpu0_0 temp (thermal zone 3)',
            units: ' (degC)'
        },
        {
            id: 'gl_tmp_cpu0_1',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 2.2,
            name: 'Glass Left board cpu0_1 temp (thermal zone 4)',
            units: ' (degC)'
        },
        {
            id: 'gl_tmp_cpu1_0',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 2.3,
            name: 'Glass Left board cpu1_0 temp (thermal zone 5)',
            units: ' (degC)'
        },
        {
            id: 'gl_tmp_cpu1_1',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 2.4,
            name: 'Glass Left board cpu1_1 temp (thermal zone 6)',
            units: ' (degC)'
        },
        {
            id: 'gl_tmp_cpu2_0',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 2.5,
            name: 'Glass Left board cpu2_0 temp (thermal zone 8)',
            units: ' (degC)'
        },
        {
            id: 'gl_tmp_cpu2_1',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 2.6,
            name: 'Glass Left board cpu2_1 temp (thermal zone 9)',
            units: ' (degC)'
        },
        {
            id: 'gl_tmp_cpu3_0',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 2.7,
            name: 'Glass Left board cpu3_0 temp (thermal zone 10)',
            units: ' (degC)'
        },
        {
            id: 'gl_tmp_cpu3_1',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 2.8,
            name: 'Glass Left board cpu3_1 temp (thermal zone 11)',
            units: ' (degC)'
        },
        {
            id: 'gl_tmp_gpu_0',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 2.81,
            name: 'Glass Left board gpu_0 temp (thermal zone 14)',
            units: ' (degC)'
        },
        {
            id: 'gl_tmp_gpu_1',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 2.82,
            name: 'Glass Left board gpu_1 temp (thermal zone 15)',
            units: ' (degC)'
        },
        {
            id: 'gl_tmp_junc_2_3',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 2.83,
            name: 'Glass Left board junction_2_3 temp (thermal zone 12)',
            units: ' (degC)'
        },
        {
            id: 'gl_amb_tmp',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 2.83,
            name: 'Glass Left ambient temperature',
            units: ' (degC)'
        },
        {
            id: 'gr_display_tmp',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 2,
            name: 'Glass Right board Display Temperature',
            units: ' (B)'
        },
        {
            id: 'gr_amb_tmp',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 2.9,
            name: 'Glass Right ambient temperature',
            units: ' (degC)'
        },
        {
            id: 'gr_dfree',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 3,
            name: 'Glass Right board free disk space',
            units: ' (B)'
        },
        {
            id: 'gr_dspace',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 3.01,
            name: 'Glass Right board disk space',
            units: ' (B)'
        },
        {
            id: 'gr_l_avg_1',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 3.02,
            name: 'Glass Right board load avg (1 min)',
            units: ' (mLAV)'
        },
        {
            id: 'gr_l_avg_15',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 3.03,
            name: 'Glass Right board load avg (15 min)',
            units: ' (mLAV)'
        },
        {
            id: 'gr_l_avg_5',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 3.04,
            name: 'Glass Right board load avg (5 min)',
            units: ' (mLav)'
        },
        {
            id: 'gr_tmp_cpu0_0',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 3.1,
            name: 'Glass Right board cpu0_0 temp (thermal zone 3)',
            units: ' (degC)'
        },
        {
            id: 'gr_tmp_cpu0_1',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 3.2,
            name: 'Glass Right board cpu0_1 temp (thermal zone 4)',
            units: ' (degC)'
        },
        {
            id: 'gr_tmp_cpu1_0',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 3.3,
            name: 'Glass Right board cpu1_0 temp (thermal zone 5)',
            units: ' (degC)'
        },
        {
            id: 'gr_tmp_cpu1_1',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 3.4,
            name: 'Glass Right board cpu1_1 temp (thermal zone 6)',
            units: ' (degC)'
        },
        {
            id: 'gr_tmp_cpu2_0',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 3.5,
            name: 'Glass Right board cpu2_0 temp (thermal zone 8)',
            units: ' (degC)'
        },
        {
            id: 'gr_tmp_cpu2_1',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 3.6,
            name: 'Glass Right board cpu2_1 temp (thermal zone 9)',
            units: ' (degC)'
        },
        {
            id: 'gr_tmp_cpu3_0',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 3.7,
            name: 'Glass Right board cpu3_0 temp (thermal zone 10)',
            units: ' (degC)'
        },
        {
            id: 'gr_tmp_cpu3_1',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 3.8,
            name: 'Glass Right board cpu3_1 temp (thermal zone 11)',
            units: ' (degC)'
        },
        {
            id: 'gr_tmp_gpu_0',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 3.81,
            name: 'Glass Right board gpu_0 temp (thermal zone 14)',
            units: ' (degC)'
        },
        {
            id: 'gr_tmp_gpu_1',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 3.82,
            name: 'Glass Right board gpu_1 temp (thermal zone 15)',
            units: ' (degC)'
        },
        {
            id: 'gr_tmp_junc_2_3',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 3.83,
            name: 'Glass Right board junction_2_3 temp (thermal zone 12)',
            units: ' (degC)'
        },
        {
            id: 'humidity',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Ambient',
            order: 1,
            name: 'Relative Humidity',
            units: ' (%)'
        },
        {
            id: 'p_display_tmp',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 2,
            name: 'Pebble board Display Temperature',
            units: ' (degC)'
        },

        {
            id: 'p_amb_tmp',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 3.9,
            name: 'Pebble board ambient temperature',
            units: ' (degC)'
        },
        {
            id: 'p_dfree',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 4,
            name: 'Pebble board board free disk space',
            units: ' (B)'
        },
        {
            id: 'p_dspace',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 4.01,
            name: 'Pebble board board disk space',
            units: ' (B)'
        },
        {
            id: 'p_l_avg_1',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 4.02,
            name: 'Pebble board board load avg (1 min)',
            units: ' (mLAV)'
        },
        {
            id: 'p_l_avg_15',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 4.03,
            name: 'Pebble board board load avg (15 min)',
            units: ' (mLAV)'
        },
        {
            id: 'p_l_avg_5',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 4.04,
            name: 'Pebble board board load avg (5 min)',
            units: ' (mLav)'
        },
        {
            id: 'p_tmp_cpu0_0',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 4.1,
            name: 'Pebble board board cpu0_0 temp (thermal zone 3)',
            units: ' (degC)'
        },
        {
            id: 'p_tmp_cpu0_1',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 4.2,
            name: 'Pebble board board cpu0_1 temp (thermal zone 4)',
            units: ' (degC)'
        },
        {
            id: 'p_tmp_cpu1_0',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 4.3,
            name: 'Pebble board board cpu1_0 temp (thermal zone 5)',
            units: ' (degC)'
        },
        {
            id: 'p_tmp_cpu1_1',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 4.4,
            name: 'Pebble board board cpu1_1 temp (thermal zone 6)',
            units: ' (degC)'
        },
        {
            id: 'p_tmp_cpu2_0',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 4.5,
            name: 'Pebble board board cpu2_0 temp (thermal zone 8)',
            units: ' (degC)'
        },
        {
            id: 'p_tmp_cpu2_1',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 4.6,
            name: 'Pebble board board cpu2_1 temp (thermal zone 9)',
            units: ' (degC)'
        },
        {
            id: 'p_tmp_cpu3_0',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 4.7,
            name: 'Pebble board board cpu3_0 temp (thermal zone 10)',
            units: ' (degC)'
        },
        {
            id: 'p_tmp_cpu3_1',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 4.8,
            name: 'Pebble board board cpu3_1 temp (thermal zone 11)',
            units: ' (degC)'
        },
        {
            id: 'p_tmp_gpu_0',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 4.81,
            name: 'Pebble board board gpu_0 temp (thermal zone 14)',
            units: ' (degC)'
        },
        {
            id: 'p_tmp_gpu_1',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 4.82,
            name: 'Pebble board board gpu_1 temp (thermal zone 15)',
            units: ' (degC)'
        },
        {
            id: 'p_tmp_junc_2_3',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 4.83,
            name: 'Pebble board board junction_2_3 temp (thermal zone 12)',
            units: ' (degC)'
        },
        {
            id: 'tmp_bot',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 4.84,
            name: 'Bottom Sensor Temp',
            units: ' (degC)'
        },
        {
            id: 'tmp_top',
            type: ['Kiosk'],
            model: ['vdkmaster' ],
            category: 'Diagnostic',
            order: 4.85,
            name: 'Top Sensor Temp',
            units: ' (degC)'
        }
        ]
    }
};

module.exports = sensors;