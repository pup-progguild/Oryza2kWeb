<?php
 /**
 * User_model
 *
 * @package	    User_model
 * @subpackage	Model
 * @category	Database Model
 * @author	    hoshi~
 * @link	    https://github.com/awkwardusername
 * @date        8/28/13 | 4:18 AM
 */

class User_model extends CI_Model {
    var $USERS_TABLE = 'users';

    var $username = '';
    var $password = '';
    var $email = '';

    public function __construct() {
        parent::__construct();

        $this->load->database();
    }

    public function add() {
        $this->input->post('username');
        $this->input->post('password');
        $this->input->post('email');

        $this->db->insert($this->USERS_TABLE, $this);
    }

    public function update() {
        $this->input->post('username');
        $this->input->post('password');
        $this->input->post('email');

        $this->db->update($this->USERS_TABLE, $this, array('id' => $this->input->post('id')));
    }

    public function update_username() {
        $this->input->post('username');

        $this->db->update($this->USERS_TABLE, $this, array('id' => $this->input->post('id')));
    }

        public function update_password() {
        $this->input->post('password');

        $this->db->update($this->USERS_TABLE, $this, array('id' => $this->input->post('id')));
    }

    public function update_email() {
        $this->input->post('email');

        $this->db->update($this->USERS_TABLE, $this, array('id' => $this->input->post('id')));
    }
}