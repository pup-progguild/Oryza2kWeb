<?php
 /**
 * Weather_data_model
 *
 * @package	    Weather data model
 * @subpackage	TODO
 * @category	Database Model
 * @author	    hoshi~
 * @link	    https://github.com/awkwardusername
 * @date        8/28/13 | 8:23 PM
 */

class Weather_data_model extends CI_Model {
    var $WEATHER_DATA_TABLE = 'weather_data';
    var $country = '';
    var $year = '';
    var $data = '';

    public function __construct() {
        parent::__construct();

        $this->load->database();
    }

    public function add() {
        $this->input->post('country');
        $this->input->post('year');
        $this->input->post('data');

        $this->db->insert($this->WEATHER_DATA_TABLE, $this);
    }

    public function update() {
        $this->input->post('country');
        $this->input->post('year');
        $this->input->post('data');

        $this->db->update($this->WEATHER_DATA_TABLE, $this, array('id' => $this->input->post('id')));
    }

    public function get_weather_data($country = FALSE, $year = FALSE) {
        if($country === FALSE && $year === FALSE) {
            $this->db->start_cache();
            $this->db->select('country', 'year');
            $this->db->stop_cache();

            $query = $this->db->get($this->WEATHER_DATA_TABLE);

            return $query->result_array();

        } elseif ($country === FALSE) {

            $query = $this->db->get_where($this->WEATHER_DATA_TABLE, array('year' => $year));
            return $query->result_array();

        } elseif ($year === FALSE) {

            $query = $this->db->get_where($this->WEATHER_DATA_TABLE, array('country' => $country));
            return $query->result_array();
        }

        $query = $this->db->get_where($this->WEATHER_DATA_TABLE, array(
            'country' => $country,
            'year' => $year
        ));

        return $query->results_array();
    }
}
