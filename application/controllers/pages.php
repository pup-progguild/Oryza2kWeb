<?php
/**
 * Created by IntelliJ IDEA.
 * User: hoshi~
 * Date: 8/24/13
 * Time: 5:29 PM
 * To change this template use File | Settings | File Templates.
 */

class Pages extends CI_Controller {
    public function view($page = 'home') {
        if(!file_exists('application/views/pages/'.$page.'.php')) {
            show_404();
        }

        $data['title'] = ucfirst($page);

        $this->load->view('templates/header', $data);
        $this->load->view('pages/'.$page, $data);
        $this->load->view('templates/footer', $data);
    }
}