<?php
 /**
 * Run_templates_data_model
 *
 * @package	    Run templates data model
 * @description A run template contains all the input data for a specified variety. Because the user can save their own
 *                  inputs, a better depiction of a custom input, hence, $label
 * @author	    hoshi~
 * @link	    https://github.com/awkwardusername
 * @date        8/28/13 | 8:28 PM
 */

class Run_templates_data_model extends CI_Model {
    var $RUN_TEMPLATES_DATA_TABLE = 'run_templates_data';

    var $label = '';                // refers to 'variety'. set as 'label' because it pertains to a set of input for a run
    var $description = '';          // 'description' of template
    var $control_dat = '';          // control.dat
    var $experiment_data_dat = '';  // experiment_data.exp
    var $crop_data_dat = '';        // crop_data.crp
    var $reruns_dat = '';           // reruns.dat
    var $preset = '';               // if a template is regarded as a 'variety' by IRRI.

    public function __construct() {
        parent::__construct();

        $this->load->database();
    }

    /*
     * @function         add()
     * @description      generic addition of row data to table
     */
    public function add() {
        $this->input->post('label');
        $this->input->post('description');
        $this->input->post('control_dat');
        $this->input->post('experiment_data_dat');
        $this->input->post('crop_data_dat');
        $this->input->post('reruns_dat');
        $this->input->post('preset');

        $this->db->insert($this->RUN_TEMPLATES_DATA_TABLE, $this);
    }

    /*
     * @function         update()
     * @description      generic update of row data to table
     */
    public function update() {
        $this->input->post('label');
        $this->input->post('description');
        $this->input->post('control_dat');
        $this->input->post('experiment_data_dat');
        $this->input->post('crop_data_dat');
        $this->input->post('reruns_dat');
        $this->input->post('preset');

        $this->db->update($this->RUN_TEMPLATES_DATA_TABLE, $this, array('id' => $this->input->post('id')));
    }

    /*
     * @function         get_a_template
     * @description      returns template data for desired $label. if not specified, all table data is returned.
     * @params           $label
     */
    public function get_template($label = FALSE) {
        if($label === FALSE) {
            $query = $this->db->get($this->RUN_TEMPLATES_DATA_TABLE);
            return $query->result_array();
        }

        $query = $this->db->get_where($this->RUN_TEMPLATES_DATA_TABLE, 'label' , $label);
        return $query->row_array();
    }

    /*
     * @function         get_template_labels
     * @description      returns the labels for all presets. if not specified, all template labels are returned.
     * @params           $preset
     */
    public function get_template_labels($preset = FALSE) {
        if($preset === FALSE) {
            $this->db->start_cache();
            $this->db->select('label');
            $this->db->stop_cache();

            $query = $this->db->get($this->RUN_TEMPLATES_DATA_TABLE);

            return $query->result_array();
        }

        $this->db->start_cache();
        $this->db->select('label')->where('preset', '1');
        $this->db->stop_cache();

        $query = $this->db->get($this->RUN_TEMPLATES_DATA_TABLE);

        return $query->result_array();
    }
    /*
     * @function         get_template_description
     * @description      returns the descriptions for all presets. if not specified, all template descriptions are returned.
     * @params           $preset
     */
    public function get_template_descriptions($preset = FALSE) {
        if($preset === FALSE) {
            $this->db->start_cache();
            $this->db->select('description');
            $this->db->stop_cache();

            $query = $this->db->get($this->RUN_TEMPLATES_DATA_TABLE);

            return $query->result_array();
        }

        $this->db->start_cache();
        $this->db->select('description')->where('preset', '1');
        $this->db->stop_cache();

        $query = $this->db->get($this->RUN_TEMPLATES_DATA_TABLE);

        return $query->result_array();
    }

    /*
    * @function         set_as_preset
    * @description      sets if a template is a preset.
    */
    public function set_as_preset() {
        $this->input->post('preset');

        $this->db->update($this->RUN_TEMPLATES_DATA_TABLE, $this, array('id' => $this->input->post('id')));
    }
}