<?php
 /**
 * Weather_data_model
 *
 * @package	    Weather data model
 * @description	The weather data for a site location, as measured in the year specified. $data also includes
  *                 the specific lat. long. location of site, so integration with Google Maps is possible as
  *                 a feature.
 * @author	    hoshi~
 * @link	    https://github.com/awkwardusername
 * @date        8/28/13 | 8:23 PM
 */

class Weather_data_model extends CI_Model {
    var $WEATHER_DATA_TABLE = 'weather_data';

    var $country = '';          // refers to the site where the weather data was obtained
    var $station_code = '';         // refers to the site station code
    var $year = '';             // the year the weather data was obtained
    var $data = '';             // the actual data itself

    public function __construct() {
        parent::__construct();

        $this->load->database();
    }

    /*
     * @function         add()
     * @description      generic addition of row data to table
     */
    public function add() {
        $this->input->post('country');
        $this->input->post('station_code');
        $this->input->post('year');
        $this->input->post('data');

        $this->db->insert($this->WEATHER_DATA_TABLE, $this);
    }

    /*
     * @function         update()
     * @description      generic update of row data to table
     */
    public function update() {
        $this->input->post('country');
        $this->input->post('station_code');
        $this->input->post('year');
        $this->input->post('data');

        $this->db->update($this->WEATHER_DATA_TABLE, $this, array('id' => $this->input->post('id')));
    }

    /*
     * @function         get_weather_data
     * @description      returns weather data for desired $country and $year. if both aren't specified,
     *                              all table data is returned. if $year is only specified, fetches all
     *                              row that matches year, and the same goes for $country.
     * @params           $country, $year
     */
    public function get_weather_data($country = FALSE, $year = FALSE) {
        if($country === FALSE && $year === FALSE) {
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

    public function get_weather_from_country_all_years($country_code) {
        $this->db->start_cache();
        $this->db->where('country_code', $country_code);
        $this->db->stop_cache();

        $query = $this->db->get($this->WEATHER_DATA_TABLE);

        return $query->result_array();
    }

    public function get_weather_from_country_till_selected_years($country_code, $year) {
        $this->db->start_cache();
        $this->db->where(array('country_code' => $country_code,
                               'year <='         => $year));
        $this->db->stop_cache();

        $query = $this->db->get($this->WEATHER_DATA_TABLE);

        return $query->result_array();
    }

    /*
     * @function         get_country_location_list
     * @description      returns 'country' => 'year' value pairs
     */
    public function get_country_year_list() {
        $this->db->start_cache();
        $this->db->select('country, year');
        $this->db->stop_cache();

        $query = $this->db->get($this->WEATHER_DATA_TABLE);

        return $query->result_array();
    }

    public function get_station_code($country_code) {
        $this->db->select('station_code, country_code')->where('country_code', $country_code);

        $query = $this->db->get($this->WEATHER_DATA_TABLE);

        return $query->row_array();
    }

    /*
     * @function         get_countries
     * @description      returns countries and their country codes
     */
    public function get_countries() {
        $this->db->start_cache();
        $this->db->select('country, country_code')->order_by('country','asc');
        $this->db->stop_cache();

        $query = $this->db->get($this->WEATHER_DATA_TABLE);

        $this->db->distinct();
        return $query->result_array();
    }

    public function get_first_country() {
        $this->db->start_cache();
        $this->db->select('country')->order_by('country', 'asc')->limit(1);
        $this->db->stop_cache();

        $query = $this->db->get($this->WEATHER_DATA_TABLE);

        return $query->row_array();
    }

    public function get_last_country() {
        $this->db->start_cache();
        $this->db->select('country')->order_by('country', 'desc')->limit(1);
        $this->db->stop_cache();

        $query = $this->db->get($this->WEATHER_DATA_TABLE);

        return $query->row_array();
    }
    /*
     * @function          get_years
     * @description       returns years
     */
    public function get_years() {
        $this->db->start_cache();
        $this->db->order_by('year','asc');
        $this->db->stop_cache();

        $this->db->distinct();
        $query = $this->db->get($this->WEATHER_DATA_TABLE);

        return $query->result_array();
    }

    public function get_last_year() {
        $this->db->start_cache();
        $this->db->select('year')->order_by('year','desc')->limit(1);
        $this->db->stop_cache();

        $query = $this->db->get($this->WEATHER_DATA_TABLE);

        return $query->row_array();
    }

    public function get_first_year() {
        $this->db->start_cache();
        $this->db->select('year')->order_by('year','asc')->limit(1);
        $this->db->stop_cache();

        $query = $this->db->get($this->WEATHER_DATA_TABLE);

        return $query->row_array();
    }

    public function count_till_first_year($last_year) {
        $this->db->start_cache();
        $this->db->distinct()->where('year >=', $last_year);
        $this->db->stop_cache();

        return $this->db->count_all_results($this->WEATHER_DATA_TABLE);
    }
}
