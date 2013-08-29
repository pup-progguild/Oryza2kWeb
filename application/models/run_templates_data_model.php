<?php
 /**
 * Run_templates_data_model
 *
 * @package	    Run templates data model
 * @subpackage	TODO
 * @category	Category
 * @author	    hoshi~
 * @link	    https://github.com/awkwardusername
 * @date        8/28/13 | 8:28 PM
 */

class Run_templates_data_model extends CI_Model {
    var $RUN_TEMPLATES_DATA_TABLE = 'run_templates_data';

    var $label = '';
    var $description = '';
    var $control_dat = '';
    var $experiment_data_dat = '';
    var $crop_data_dat = '';
    var $reruns_dat = '';
    var $preset = '';

    public function __construct() {
        parent::__construct();

        $this->load->database();
    }

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

    public function get_a_template($label = FALSE) {
        if($label === FALSE) {
            $query = $this->db->get($this->RUN_TEMPLATES_DATA_TABLE);
            return $query->result_array();
        }

        $query = $this->db->get_where($this->RUN_TEMPLATES_DATA_TABLE, 'label' , $label);
        return $query->row_array();
    }

    public function get_template_labels($label = FALSE) {
        if($label === FALSE) {
            $this->db->start_cache();
            $this->db->select('label');
            $this->db->stop_cache();

            $query = $this->db->get($this->RUN_TEMPLATES_DATA_TABLE);

            return $query->result_array();
        }

        $this->db->start_cache();
        $this->db->select('label')->where('label' , $label);
        $this->db->stop_cache();

        $query = $this->db->get($this->RUN_TEMPLATES_DATA_TABLE);

        return $query->result_array();
    }

    public function get_template_labels_preset($label = FALSE) {
        if($label === FALSE) {
            $this->db->start_cache();
            $this->db->select('label')->where('preset', '1');
            $this->db->stop_cache();

            $query = $this->db->get($this->RUN_TEMPLATES_DATA_TABLE);

            return $query->result_array();
        }

        $this->db->start_cache();
        $this->db->select('label')->where(array('label' => $label, 'preset' => '1'));
        $this->db->stop_cache();

        $query = $this->db->get($this->RUN_TEMPLATES_DATA_TABLE);

        return $query->result_array();
    }

    public function set_as_preset() {
        $this->input->post('preset');

        $this->db->update($this->RUN_TEMPLATES_DATA_TABLE, $this, array('id' => $this->input->post('id')));
    }
}